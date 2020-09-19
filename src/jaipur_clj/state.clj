;; state.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.state
  (:require [clojure.pprint :as pp]
            [clojure.spec.alpha :as s]
            [spec-dict :refer [dict dict*]]))

;; -------------------------------------
;; Spec definitions
(s/def ::cards (dict {:diamond int?
                      :gold int?
                      :silver int?
                      :cloth int?
                      :spice int?
                      :leather int?
                      :camel int?}))
(s/def ::tokens (dict {:diamond (s/* int?)
                       :gold (s/* int?)
                       :silver (s/* int?)
                       :cloth (s/* int?)
                       :spice (s/* int?)
                       :leather (s/* int?)
                       :camel (s/* int?)}))

;; -------------------------------------
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

;; -------------------------------------
; initial-state :: State
(s/def ::state (dict {:deck ::cards
                      :market ::cards
                      :hand (dict {:a ::cards :b ::cards})
                      :points (dict {:a int? :b int?})
                      :tokens ::tokens}))

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
;; Utilities
 
(defn min-sell
  "The minimum sell number for a resource"
  [rsrc]
  (let [min-sell-hash {:diamond 2 :gold 2 :silver 2
                       :cloth 1 :spice 1 :leather 1
                       :camel 99}]
    (rsrc min-sell-hash)))

(defn count-cards-excl-camels
  "Count call the cards in a hand, excluding the camels"
  [hand]
  (- (apply + (vals hand))
     (:camel hand)))

;; hand-values :: Player -> State -> Vector Integer
(defn hand-values
  "Return the vector of the number of each card in a hand."
  [player state]
  (vec (vals (get-in state [:hand player]))))

;; token-values :: State -> Vector Integer
(defn token-values
  "Return the vector of sums of the tokens."
  [state]
  (mapv (partial apply +) (vals (:tokens state))))

(defn- safe-div
  "Safe division that forces x/0 to 0."
  [x y]
  (if (zero? y) 0 (/ x y)))

;; mean-token-values :: State -> Vector Rational
(defn mean-token-values
  "Return the vector of mean of the tokens. If the number of tokens is zero
  then set that result to zero."
  [state]
  (let [token-vals ((comp butlast vals :tokens) state)]
    (mapv #(safe-div (apply + %) (count %))
          token-vals)))

;; encode-state :: Player -> State -> Vector Integer
(defn encode-state
  "Encode the visible state for a given player as a numeric vector of length 21."
  [plyr state]
  (let [deck (apply + (vals (:deck state))) ; number of deck cards (1)
        hand (vals (get-in state [:hand plyr])) ; hand cards (7)
        market (vals (:market state)) ; market cards (7)
        tokens (vals (:tokens state)) ; sum of each token pile (6)
        points (get-in state [:points plyr])] ; points for that player (1)
    (->> (list deck
               hand
               market
               (map #(apply + %) tokens)
               points)
         concat
         flatten)))

(defn- range-seq
  "Return the items m to n inclusive from lst."
  [m n lst]
  (->> lst
       (take (inc n))
       (drop m)))

(defn print-state
  "Generate a string version of the encoded state for logging."
  [plyr state]
  (let [enc (encode-state plyr state)]
    (list (first enc)
          (range-seq 1 7 enc)
          (range-seq 8 14 enc)
          (range-seq 15 20 enc)
          (last enc))))

(defn ppst
  "Pretty print the state."
  [st]
  {:pre (s/valid? ::state st)}
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
