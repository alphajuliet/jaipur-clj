;; state.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.core
  (:require [lentes.core :as l]))

(def empty-hand {:diamond 0
                 :gold 0
                 :silver 0
                 :cloth 0
                 :spice 0
                 :leather 0
                 :camel 0})

(def all-cards {:diamond 6
                :gold 6
                :silver 6
                :cloth 8
                :spice 8
                :leather 10
                :camel 11})

; initial-state :: State
(def initial-state
  {:deck all-cards
   :market empty-hand
   :hand {:a empty-hand
          :b empty-hand}
   :points {:a 0
            :b 0}
   :tokens {:diamond [7 7 5 5 5]
            :gold [6 6 5 5 5]
            :silver [5 5 5 5 5]
            :cloth [5 3 3 2 2 1 1]
            :spice [5 3 3 2 2 1 1]
            :leather [4 3 2 1 1 1 1 1 1]
            :camel []}})

; Lenses
(def _deck (l/key :deck))
(def _market (l/key :market))
(def _hand (l/key :hand))
(def _points (l/key :points))
(def _tokens (l/key :tokens))
;(defn _rsrc [r] (l/key r))
;(defn _player [p] (l/key p))

;; -------------------------------------
; Minimum sell quantities
(def min-sell-hash
  {:diamond 2 :gold 2 :silver 2
   :cloth 1 :spice 1 :leather 1
   :camel 99})

(defn min-sell 
  "The minimum sell number for a resource"
  [rsrc]
  (rsrc min-sell-hash))

(defn count-cards-excl-camels 
  "Count call the cards in a hand, excluding the camels"
  [hand]
  (- (h/hash-sum hand) 
     (:camel hand)))


; The End