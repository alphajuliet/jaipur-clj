;; policy.clj
;; Game playing policies
;; AndrewJ 2019-09-28

(ns jaipur-clj.policy
  (:require [jaipur-clj.state :refer :all]
            [jaipur-clj.actions :refer :all]
            [jaipur-clj.game :refer :all]
            [jaipur-clj.hash-calc :as h]
            [clojure.java.io :as io]))

;;-------------------------------
;; Logging setup

(def log-file-name "game.txt")
(io/delete-file log-file-name :quiet)

(defn log
  "Print a string to a log file."
  [s]
  (spit log-file-name s :append true)
  (spit log-file-name "\n" :append true))

;;-------------------------------
;; Utility functions

; random-value :: Hash a b -> b
(def random-value (comp rand-nth vals))

(defn argmax-map
  "Find the key with the maximum value."
  [m]
  (key (apply max-key val m)))

(defn argmax
  "Return the value x in xs that maximises (f x)."
  [f xs]
  (apply max-key f xs))

(defn argmin
  "Return the value x in xs that minimises (f x)."
  [f xs]
  (apply min-key f xs))

; From https://stackoverflow.com/questions/1601321/idiomatic-mode-function-in-clojure
(defn tally-map
  " Create a map where the keys are all of the unique elements in the input
   sequence and the values represent the number of times those elements
   occur. Note that the keys may not be formatted as conventional Clojure
   keys, i.e. a colon preceding a symbol."
  [aseq]
  (apply merge-with + (map (fn [x] {x 1}) aseq)))

(defn dot-product
  "Dot product of two vectors."
  [v1 v2]
  {:pre [(= (count v1) (count v2))]}
  (reduce + (map * v1 v2)))

;-------------------------------
; type Policy = Player -> State -> Action
; apply-policy :: Policy -> Player -> State -> State
(defn apply-policy
  "Apply a given policy function to generate the next state."
  [policy player st]
  (let [action (policy player st)
        new-state (apply-action action st)]
    (log action)
    (log (print-state player new-state))
    new-state))

;-------------------------------
; play-game :: Policy -> State -> State
(defn play-game
  "Play a game with a policy function for each player. 
   Limit the number of turns per player to `max-turns` with default 100."
  ([policy-a policy-b initial-state]
   (play-game policy-a policy-b initial-state 100))

  ([policy-a policy-b initial-state max-turns]
   ; Log the initial state'
   (log (str policy-a))
   (log (str policy-b))
   (log (print-state :a initial-state))
   (log (print-state :b initial-state))

  ; Iterate through the actions for each player to generate a final state
   (reduce
    (fn [state i]
      (if (end-of-game? state)
        (reduced (let [final-state (apply-end-bonus state)]
                   (log "---- Final state")
                   (log (print-state :a final-state))
                   (log (print-state :b final-state))
                   final-state))
        ;else
        (do
          (log (format "---- Iteration %d:" i))
          (->> state
               (apply-policy policy-a :a)
               (apply-policy policy-b :b)))))
    initial-state
    (range max-turns))))

(defn winner
  "Identify the winner"
  [st]
  (argmax-map (:points st)))

(defn play-n-games
  "Play n games using the same policies and initial state, and aggregate the wins."
  [n policy-a policy-b initial-state]
  (tally-map
   (reduce (fn [s _]
             (conj s (winner (play-game policy-a policy-b initial-state))))
           []
           (range n))))

;;-------------------------------
;; Policies

; random-policy :: Player -> State -> Action
(defn random-policy
  "Choose a random action from the ones available."
  [player state]
  (->> (available-actions player state)
       (group-by first)
       (random-value)
       (rand-nth)))

(defn- score-points
  "Score the number of points."
  [player action state]
  (let [next-state (apply-action action state)]
    (get-in next-state [:points player])))

; greedy-policy :: Player -> State -> Action
(defn greedy-policy
  "Choose the available action that maximises the points in the target states. If none, then pick a random one."
  [player state]
  (argmax #(score-points player % state) 
          (shuffle (available-actions player state))))

; score-a :: Player -> Action -> State -> Integer
(defn- score-points-delta
  "Measure the points difference between two states."
  [player action state]
  (let [next-state (apply-action action state)]
    (- (get-in next-state [:points player])
       (get-in state [:points player]))))

; alpha-policy :: Player -> State -> Action
(defn alpha-policy
  "Maximise delta of points between current and next state."
  [player state]
  (argmax #(score-points-delta player % state) 
          (available-actions player state)))

; token-hand-gap :: Player -> State -> Integer
(defn- score-token-hand-gap
  "Measure the gap between the remaining tokens and the player's hand."
  [player action state]
  (let [next-state (apply-action action state)
        hand (get-in next-state [:hand player])
        tokens (:tokens next-state)
        tsums (zipmap (keys tokens)
                      (map (partial reduce +) (vals tokens)))]
    (h/hash-sum (h/hash-sub tsums hand))))

; beta-policy :: Player -> State -> Action
(defn beta-policy
  "Maximise the value in the player's hand."
  [player state]
  (argmin #(score-token-hand-gap player % state)
          (available-actions player state)))

(defn- score-dotp
  "Calculate the dot product of each available token against each hand card type."
  [player action state]
  (let [next-state (apply-action action state)]
    (dot-product (hand-values player next-state)
                 (token-values next-state))))

; gamma-policy :: Player -> State -> Action
(defn gamma-policy
  "Maximise the 'value' of cards in the hand against the tokens remaining."
  [player state]
  (argmax #(score-dotp player % state)
          (available-actions player state)))

;; The End