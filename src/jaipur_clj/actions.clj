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

;; The End