;; actions.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.actions
  (:require [jaipur-clj.state :refer :all]
            [lentes.core :as l]
            [jaipur-clj.hash-calc :as h]
            [random-seed.core :as r])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;-------------------------------
; random-card :: State -> Resource
(defn- random-card
  "Random card from the deck"
  [st]
  (->> (:deck st)
       (h/hash-enumerate)
       (r/rand-nth)))

;-------------------------------
; move-cards :: Resource -> Lens -> Lens -> Int -> State -> State
(defn move-cards
  "Move n cards from _src to _dest"
  [rsrc _src _dest n st]
  (assert (<= n (l/focus (comp _src (l/key rsrc)) st)))
  (->> st
       (l/over (comp _src (l/key rsrc)) #(- % n))
       (l/over (comp _dest (l/key rsrc)) #(+ % n))))

; Deal n cards from the deck to the target
; deal-cards :: Lens -> Int -> State -> State
(defn deal-cards
  "Deal n cards from the deck to the target"
  [_target n state]
  (let [st state
        n1 (min n (h/hash-sum (:deck state)))] ;only deal as many as are left
    (reduce
     (fn [s _]
       (move-cards (random-card s) _deck _target 1 s))
     st (range n1))))

(defn- bonus-points
  "Calculate bonus points for 3+ cards"
  [n]
  (cond
    (= n 3) (r/rand-nth '(1 2 3))
    (= n 4) (r/rand-nth '(4 5 6))
    (= n 5) (r/rand-nth '(8 9 10))
    :else 0))

; take-tokens :: Resource -> Player -> Int -> State -> State
(defn- take-tokens
  "Take n resource tokens and add to player's score"
  [rsrc plyr n st]

  (let [t (get-in st [:tokens rsrc])
        v (if (>= n (count t)) ; Return the tokens, split into two
            [t []]
            (split-at n t))]
    (-> st
        (update-in [:points plyr]
                   #(+ % (apply + (first v)) (bonus-points n)))
        (assoc-in [:tokens rsrc]
                  (into [] (second v))))))

;===============================
; Game actions
; - Init game
; - Take cards
; - Sell cards
; - Exchange cards

;-------------------------------
(defn init-game
; init-game :: Int? -> State
  "Initialise the game, with an optional seed > 0"
  ([]
   (->> initial-state
        (move-cards :camel _deck _market 3)
        (deal-cards _market 2)
        (deal-cards (l/in [:hand :a]) 5)
        (deal-cards (l/in [:hand :b]) 5)))
  ([seed]
   (r/set-random-seed! seed)
   (init-game)))

;-------------------------------
; Take a card from the market (or all the camels)
; Deal replacement cards to the deck

; Invalid if:
; - Player is not taking camels, and already has 7 non-camel cards in their hand

; take-card-invalid? :: Resource -> Player -> State -> Boolean | String
(defn take-card-invalid?
  "Confirm whether the take-card action is valid."
  [rsrc plyr st]
  (let [player-hand (get-in st [:hand plyr])]
    (if (and (not (= rsrc :camel))
             (> (count-cards-excl-camels player-hand) 7))
      (format "Player %s cannot have more than 7 cards, excluding camels." plyr)
      false)))

; take-card :: Resource -> Player -> State -> State
(defn take-card
  "Take a card from the market (or all the camels), and deal replacement cards to the deck"
  [rsrc plyr st]

  (let [error (take-card-invalid? rsrc plyr st)]
    (assert (boolean? error) error))

  (let [n-market-camels (get-in st [:market :camel])
        _player-hand (comp _hand (l/key plyr))]
    (if (= rsrc :camel)
      (->> st
           (move-cards rsrc _market _player-hand n-market-camels)
           (deal-cards _market n-market-camels))
     ; else
      (->> st
           (move-cards rsrc _market _player-hand 1)
           (deal-cards _market 1)))))

;-------------------------------
; Sell cards

; sell-cards-invalid? :: Player -> Resource -> State -> Boolean | String|
(defn sell-cards-invalid?
  "Determine whether the sell-cards action is valid."
  [rsrc plyr st]
  (let [n (get-in st [:hand plyr rsrc])]
    (cond
      (= rsrc :camel) (format "Player %s cannot sell camels." plyr)
      (< n (min-sell rsrc)) (format "Player %s does not enough %s cards to sell." plyr rsrc)
      :else false)))

; sell-cards :: Player -> Resource -> State -> State
(defn sell-cards
  "Sell all the given resources in a player's hand, and take tokens."
  [rsrc plyr st]

  (let [error? (sell-cards-invalid? rsrc plyr st)]
    (assert (boolean? error?) error?))

  (let [n (get-in st [:hand plyr rsrc])]
    (->> st
         (l/over (comp _hand (l/key plyr) (l/key rsrc)) #(- % n))
         (take-tokens rsrc plyr n))))

;-------------------------------
; Exchange cards

(defn- not-enough-cards?
  "Are there enough cards in the hand?"
  [cards hand]
  (neg? (h/hash-min (h/hash-sub hand cards))))

(defn exchange-cards-invalid?
  "Determine if the exchange-cards action is valid."
  [player-cards market-cards plyr st]

  (let [player-hand (get-in st [:hand plyr])]
    (cond
      (not (= (h/hash-sum player-cards) (h/hash-sum market-cards)))
      "Different number of resources being exchanged."
      (or (not-enough-cards? player-cards (get-in st [:hand plyr]))
          (not-enough-cards? market-cards (:market st)))
      "Cannot exchange resources that aren't available."
      (contains? market-cards :camel)
      "Cannot exchange a camel from the market."
      (> (+ (count-cards-excl-camels player-hand) (:camel player-cards 0)) 7)
      "Cannot have more than 7 hand cards after exchange."
      :else false)))

; exchange-cards ::  Cards -> Cards -> Player ->State -> State
(defn exchange-cards
  "Exchange cards with the market, including swapping for camels."
  [player-cards market-cards plyr st]

  (let [error? (exchange-cards-invalid? player-cards market-cards plyr st)]
    (assert (boolean? error?) error?))

  (-> st
        ; Move player cards to market
      (update :market #(h/hash-add % player-cards))
      (update-in [:hand plyr] #(h/hash-sub % player-cards))
          ; Move market cards to player
      (update-in [:hand plyr] #(h/hash-add % market-cards))
      (update :market #(h/hash-sub % market-cards))))

;-------------------------------
; end-of-game? :: State -> Boolean
(defn end-of-game?
  "Check for end of game
   - Deck is empty
   - Three token piles are empty"
  [st]

  (let [token-lengths (->> (:tokens st)
                           vals
                           (map count))]
    (or (= 0 (h/hash-sum (:deck st)))
        (= 3 (count (filter #(= % 0) token-lengths))))))

;-------------------------------
; apply-end-bonus :: State -> State
(defn apply-end-bonus
  "Add end-of-game bonus of 5 points for greater number of camels."
  [st]

  #_(println "Apply end bonus points.")
  (let [ca (get-in st [:hand :a :camel])
        cb (get-in st [:hand :b :camel])]
    (cond (> ca cb) (update-in st [:points :a] + 5)
          (< ca cb) (update-in st [:points :b] + 5)
          :else st)))

;; The End