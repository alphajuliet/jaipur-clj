;; game.rkt
;; Implement the game engine
;; AndrewJ 2019-09-22

(ns jaipur-clj.core
  (:require [jaipur-clj.hash-calc :as h]
            [jaipur-clj.util :refer :all]
            [clojure.math.combinatorics :as c]
            [lentes.core :as l]))

;-------------------------------
;; Utilities

(defn key-combinations [h n]
  (distinct (c/combinations (h/hash-enumerate h) n)))

(def count-if
  (comp count filter))

;-------------------------------
; take-card-options :: Player -> State -> List Action
(defn take-card-options
  "Generate the options for take-card:
   1. Camels, if any are in the market
   2. Any (non-camel) resource from the market, if player hand contains less than 7 cards"
  [plyr st]

  (def market-camels (l/focus (comp _market (l/key :camel)) st))
  (def market-cards (l/focus _market st))
  (def player-cards (l/focus (comp _hand (l/key plyr)) st))
  (def n-player-cards (count-cards-excl-camels player-cards))

  (concat
     ; case 1
   (if (> market-camels 0)
     (list `(take-card :camel ~plyr))
     '())
     ; case 2
   (for [[k v] (seq market-cards)
         :when (not= k :camel)
         :when (> v 0)
         :when (< n-player-cards 7)]
     `(take-card ~k ~plyr))))


;-------------------------------
; sell-cards-options :: Player -> State -> List Action
(defn sell-cards-options
  "Determine the options for sell-cards.
   - Any hand cards with the minimum sell quantity, apart from camels"
  [plyr st]

  (let [player-cards (l/focus (comp _hand (l/key plyr)) st)]
    (for [[k v] (seq player-cards)
          :when (not (sell-cards-invalid? k plyr st))]
      `(sell-cards ~k ~plyr))))

;-------------------------------
; sell-cards-options :: Player -> State -> List Action
(defn exchange-cards-options
  "List all the exchange-card actions.
   - Min of 2 cards
   - Does not include market camels
   - Source and target cards must be different"
  [plyr st]

  (let [player-cards (l/focus (comp _hand (l/key plyr)) st)
        market-cards (l/focus _market st)]
    (for [n (range 2 (inc (count-cards-excl-camels market-cards)))
          x (c/cartesian-product (key-combinations player-cards n)
                                 (key-combinations (dissoc market-cards :camel) n))
          :when (< (+ (count-if #(= :camel %) (first x))
                      (count-cards-excl-camels player-cards))
                   7)]
      `(exchange-cards ~(h/hash-collect (first x)) ~(h/hash-collect (second x)) ~plyr))))

;-------------------------------
(defn available-actions
  "Return all the available actions, given a player and a current state."
  [plyr st]
  (reduce concat '() 
          ((juxt take-card-options sell-cards-options exchange-cards-options)
           plyr st)))

;-------------------------------
; apply-action :: Action -> State -> State
(defn apply-action
  "Apply an action to a state"
  [action state]
  (eval (concat action (list state))))

; Standard starting game
(def s0 (init-game 0))

;; The End