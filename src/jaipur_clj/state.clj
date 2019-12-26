;; state.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.state
  (:require [jaipur-clj.hash-calc :as h]
            [clojure.pprint :as pp]))

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

(defn hand-values
  "Return the vector of hand cards."
  [player state]
  (vec (vals (get-in state [:hand player]))))

(defn token-values
  "Return the vector of sums of the tokens."
  [state]
  (mapv (partial apply +) (vals (:tokens state))))

(defn encode-state
  "Encode the visible state for a given player as a numeric vector of length 21."
  [plyr state]
  (let [deck (apply + (vals (:deck state))) ; number of deck cards
        hand (vals (get-in state [:hand plyr])) ; hand cards
        market (vals (:market state)) ; market cards
        tokens (vals (:tokens state)) ; sum of each token pile
        points (get-in state [:points plyr])] ; points for that player
    (->> (list deck
               hand 
               market 
               (map #(apply + %) tokens)
               points)
         concat
         flatten)))

(defn ppst
  "Pretty print the state."
  [st]
  (print "Deck, Market, Hand A, Hand B:")
  (pp/print-table [(:deck st)
                   (:market st)
                   (get-in st [:hand :a])
                   (get-in st [:hand :b])])
  (print "Points:")
  (pp/print-table [(:points st)])
  (println "Tokens:")
  (pp/pprint (:tokens st)))

;; The End