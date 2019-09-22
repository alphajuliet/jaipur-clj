;; actions.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.core
  (:require [lentes.core :as l]
            [jaipur-clj.hash-calc :as h]
            [jaipur-clj.util :refer :all]
            [random-seed.core :refer :all])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;-------------------------------
; Random card from the deck
; random-card :: State -> Resource
(defn random-card [st]
  (->> (l/focus _deck st)
       (h/hash-enumerate)
       (rand-nth)))

;-------------------------------
; Move n cards from _src to _dest
; move-cards :: Resource -> Lens -> Lens -> Int -> State -> State
(defn move-cards [rsrc _src _dest n st]
  (assert (<= n (l/focus (comp _src (l/key rsrc)) st)))
  (->> st
       (l/over (comp _src (l/key rsrc)) (fn [x] (- x n)))
       (l/over (comp _dest (l/key rsrc)) (fn [x] (+ x n)))))

; Deal n cards from the deck to the target
; deal-cards :: Lens -> Int -> State -> State
(defn deal-cards [_target n state]
  (let [st state
        n1 (min n (h/hash-sum (l/focus _deck state)))] ;only deal as many as are left
    (reduce
     (fn [s i]
       (move-cards (random-card s) _deck _target 1 s))
     st (range n1))))


;===============================
; Game actions
; - Init game
; - Take cards
; - Sell cards
; - Exchange cards

;-------------------------------
; Initialise the game, with an optional seed > 0
; init-game :: Int? -> State
(defn init-game
  ([]
   (->> initial-state
        (move-cards :camel _deck _market 3)
        (deal-cards _market 2)
        (deal-cards (l/in [:hand :a]) 5)
        (deal-cards (l/in [:hand :b]) 5)))
  ([seed]
   (set-random-seed! seed)
   (init-game)))

;-------------------------------
; Take a card from the market (or all the camels)
; Deal replacement cards to the deck

; Invalid if:
; - Player is not taking camels, and already has 7 non-camel cards in their hand

; take-card-invalid? :: Resource -> Player -> State -> Boolean | String
(defn take-card-invalid? [rsrc plyr st]
  (let [player-hand (l/focus (comp _hand (l/key plyr)) st)]
    (if (and (not (= rsrc :camel))
             (> (count-cards-excl-camels player-hand) 7))
      (format "Player %s cannot have more than 7 cards, excluding camels." plyr)
      false)))

; take-card :: Resource -> Player -> State -> State
(defn take-card [rsrc plyr st]

  (def n-market-camels (l/focus (comp _market (l/key :camel)) st))
  (def player-hand (l/focus (comp _hand (l/key plyr)) st))
  (def error (take-card-invalid? rsrc plyr st))

  (assert (boolean? error) error)
  (if (= rsrc :camel)
    (->> st
         (move-cards rsrc _market (comp _hand (l/key plyr)) n-market-camels)
         (deal-cards _market n-market-camels))
    ; else
    (->> st
         (move-cards rsrc _market (comp _hand (l/key plyr)) 1)
         (deal-cards _market 1))))

;; The End