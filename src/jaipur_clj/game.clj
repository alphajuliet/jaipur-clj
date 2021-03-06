;; game.rkt
;; Implement the game engine
;; AndrewJ 2019-09-22

(ns jaipur-clj.game
  (:require [jaipur-clj.state :as st]
            [jaipur-clj.actions :as act]
            [jaipur-clj.hash-calc :as h]
            [clojure.math.combinatorics :as c]))

;-------------------------------
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

;-------------------------------
; take-card-options :: Player -> State -> List Action
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
       (list `(act/take-card :camel ~plyr))
       '())
    ; case 2
     (for [[k v] (seq market-cards)
           :when (not= k :camel)
           :when (> v 0)
           :when (< n-player-cards 7)]
       `(act/take-card ~k ~plyr)))))


;-------------------------------
; sell-cards-options :: Player -> State -> List Action
(defn sell-cards-options
  "Determine the options for sell-cards.
   - Any hand cards with the minimum sell quantity, apart from camels"
  [plyr st]

  (let [player-cards (get-in st [:hand plyr])]
    (for [[k _] (seq player-cards)
          :when (not (act/sell-cards-invalid? k plyr st))]
      `(act/sell-cards ~k ~plyr))))

;-------------------------------
; sell-cards-options :: Player -> State -> List Action
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
      `(jaipur-clj.actions/exchange-cards ~(h/hash-collect (first x))
                                          ~(h/hash-collect (second x))
                                          ~plyr))))

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
(def s0 (act/init-game 0))

;; The End