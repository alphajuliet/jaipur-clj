;; policy.clj
;; Game playing policies
;; AndrewJ 2019-09-28

(ns jaipur-clj.core
  (:require [lentes.core :as l]
            [jaipur-clj.hash-calc :as h]
            [clojure.java.io :as io]))

;-------------------------------
; Logging setup

(def log-file-name "game.txt")
(io/delete-file log-file-name :quiet)

(defn log 
  "Print a string to a log file."
  [s]
  (spit log-file-name (str s "\n") :append true))

;-------------------------------
; Utility functions

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

; From https://stackoverflow.com/questions/1601321/idiomatic-mode-function-in-clojure
(defn tally-map
  " Create a map where the keys are all of the unique elements in the input
   sequence and the values represent the number of times those elements
   occur. Note that the keys may not be formatted as conventional Clojure
   keys, i.e. a colon preceding a symbol."
  [aseq]
  (apply merge-with + (map (fn [x] {x 1}) aseq)))

;-------------------------------
; type Policy = Player -> State -> Action
; apply-policy :: Policy -> Player -> State -> State
(defn apply-policy
  "Apply a given policy function to generate the next state."
  [policy plyr st]
  (let [action (policy plyr st)]
    (log action)
    (log st)
    (apply-action action st)))

;-------------------------------
; play-game :: Policy -> State -> State
(defn play-game
  "Play a game with a policy function for each player."
  ([policy-a policy-b initial-state]
   (play-game policy-a policy-b initial-state 100))

  ([policy-a policy-b initial-state max-iter]

  ; Iterate through the actions for each player to generate a final state
   (reduce
    (fn [state i]
      (cond (end-of-game? state) (reduced (apply-end-bonus state))
            :else (do
                    (log (format "---- Iteration %d:" i))
                    (->> state
                         (apply-policy policy-a :a)
                         (apply-policy policy-b :b)))))
    initial-state
    (range max-iter))))

(defn winner
  "Identify the winner"
  [st]
  (argmax-map (l/focus _points st)))

(defn play-n-games
  "Play n games using the same policies and initial state, and aggregate the wins."
  [n policy-a policy-b initial-state]
  (tally-map
   (reduce (fn [s i] (conj s (winner (play-game policy-a policy-b initial-state))))
           []
           (range n))))

;-------------------------------
; Policies

; random-policy :: Player -> State -> Action
(defn random-policy
  "Choose a random action from the ones available."
  [player state]
  (->> (available-actions player state)
       (group-by first)
       (random-value)
       (rand-nth)))

; greedy-policy :: Player -> State -> Action
(defn greedy-policy
  "Choose the available action that maximises the points in the target states."
  [player state]

  ; Helper function
  (defn- points [st]
    (l/focus (comp _points (l/key player)) st))

  (argmax #(points (apply-action % state))
          (available-actions player state)))

;; The End
