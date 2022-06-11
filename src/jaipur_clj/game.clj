;; game.rkt
;; Implement the game engine
;; AndrewJ 2019-09-22

(ns jaipur-clj.game
  (:require [clojure.math.combinatorics :as c]
            [jaipur-clj.actions :as act]
            [jaipur-clj.hash-calc :as h]
            [jaipur-clj.state :as st]
            [random-seed.core :as r]))

;;-------------------------------
;; Utilities

(defn key-combinations [h n]
  (distinct (c/combinations (h/hash-enumerate h) n)))

(def count-if
  "Count items that satisfy the filter."
  (comp count filter))

(defn common-cards
  "Find the cards common to both hands."
  [h1 h2]
  (some (set h1) h2))

;;-------------------------------
;; init-game :: Int? -> State
(defn init-game
  "Initialise the game, with an optional seed >= 0"
  ([]
   (->> st/initial-state
        (act/move-cards :camel [:deck] [:market] 3)
        (act/deal-cards [:market] 2)
        (act/deal-cards [:hand :a] 5)
        (act/deal-cards [:hand :b] 5)))

  ([seed]
   (r/set-random-seed! seed)
   (init-game)))

;;-------------------------------
;; end-of-game? :: State -> Boolean
(defn end-of-game?
  "Check for end of game
   - Deck is empty
   - Three token piles are empty"
  [st]

  (let [token-lengths (as-> st <>
                        (:tokens <>)
                        (dissoc <> :camel)
                        (vals <>)
                        (map count <>))]
    (or (= 0 (h/hash-sum (:deck st)))
        (= 3 (count (filter #(= % 0) token-lengths))))))

;;-------------------------------
;; take-card-options :: Player -> State -> List Action
(defn take-card-options
  "Generate the options for take-card:
   1. Camels, if any are in the market
   2. Any (non-camel) resource from the market, if player hand contains less than 7 cards"
  [plyr st]

  (let [market-camels (get-in st [:market :camel])
        market-cards (:market st)
        player-cards (get-in st [:hand plyr])
        n-player-cards (st/count-cards-excl-camels player-cards)]
    (concat
    ; case 1
     (if (> market-camels 0)
       (list {:action :take-card :card :camel :player plyr})
       '())
    ; case 2
     (for [[k v] (seq market-cards)
           :when (not= k :camel)
           :when (> v 0)
           :when (< n-player-cards 7)]
       {:action :take-card :card k :player plyr}))))


;;-------------------------------
;; sell-cards-options :: Player -> State -> List Action
(defn sell-cards-options
  "Determine the options for sell-cards.
   - Any hand cards with the minimum sell quantity, apart from camels"
  [plyr st]

  (let [player-cards (get-in st [:hand plyr])]
    (for [[k _] (seq player-cards)
          :when (not (act/sell-cards-invalid? k plyr st))]
      {:action :sell-cards, :card k, :player plyr})))

;;-------------------------------
;; sell-cards-options :: Player -> State -> List Action
(defn exchange-cards-options
  "List all the exchange-card actions.
   - Min of 2 cards
   - Does not include market camels
   - Source and target cards must be different"
  [plyr st]

  (let [player-cards (get-in st [:hand plyr])
        market-cards (:market st)]
    (for [n (range 2 (inc (st/count-cards-excl-camels market-cards)))
          x (c/cartesian-product (key-combinations player-cards n)
                                 (key-combinations (dissoc market-cards :camel) n))
          :when (< (+ (count-if (partial = :camel) (first x))
                      (st/count-cards-excl-camels player-cards))
                   7)
          :when (nil? (common-cards (first x) (second x)))]
      {:action :exchange-cards
       :give-cards (h/hash-collect (first x))
       :take-cards (h/hash-collect (second x))
       :player plyr})))

;;-------------------------------
;; available-actions :: Player -> State -> [Action]
(defn available-actions
  "Return all the available actions, given a player and a current state."
  [plyr st]

  (reduce concat '()
          ((juxt take-card-options sell-cards-options exchange-cards-options)
           plyr st)))

;;-------------------------------
;; apply-action :: Action -> State -> State
(defn apply-action
  "Apply an action to a state"

  [action state]
  ;; (eval (concat action (list state)))
  (case (:action action)
    :take-card (act/take-card (:card action) (:player action) state)
    :sell-cards (act/sell-cards (:card action) (:player action) state)
    :exchange-cards (act/exchange-cards (:give-cards action) (:take-cards action) (:player action) state)
    (throw (Exception. "Unknown action in function apply-action"))))

;; Standard starting game
(def s0 (init-game 0))

;; The End
