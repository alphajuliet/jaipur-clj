;; policy.clj
;; Game playing policies
;; AndrewJ 2019-09-28

(ns jaipur-clj.policy
  (:require [jaipur-clj.state :refer :all]
            [jaipur-clj.actions :refer :all]
            [jaipur-clj.game :refer :all]
            [lentes.core :as l]
            [clojure.java.io :as io]))

;-------------------------------
; Logging setup

(def log-file-name "game.txt")
(io/delete-file log-file-name :quiet)

(defn- spit-seq
  "Convert a sequence to a string in parentheses."
  [dest s]
  (spit dest
        (str "(" (clojure.string/join " " s) ")\n")
        :append true))

(defn log
  "Print a string to a log file."
  [s]
  (if (seq? s)
    (spit-seq log-file-name s)
    (spit log-file-name (str s "\n")
          :append true)))

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
  (let [action (policy plyr st)
        new-state (apply-action action st)]
    (log action)
    (log (encode-state plyr new-state))
    new-state))

;-------------------------------
; play-game :: Policy -> State -> State
(defn play-game
  "Play a game with a policy function for each player."
  ([policy-a policy-b initial-state]
   (play-game policy-a policy-b initial-state 100))

  ([policy-a policy-b initial-state max-iter]

   ; Log the initial state
   (log (encode-state :a initial-state))
   (log (encode-state :b initial-state))

  ; Iterate through the actions for each player to generate a final state
   (reduce
    (fn [state i]
      (if (end-of-game? state)
        (reduced (apply-end-bonus state))
        (do
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
   (reduce (fn [s _]
             (conj s (winner (play-game policy-a policy-b initial-state))))
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

; Helper function
(defn- points
  [player st]
  (l/focus (comp _points (l/key player)) st))

; greedy-policy :: Player -> State -> Action
(defn greedy-policy
  "Choose the available action that maximises the points in the target states. If none, then pick the final one, as per the `maxkey` function."
  [player state]
  (argmax #(points player (apply-action % state))
          (available-actions player state)))

; alpha-policy :: Player -> State -> Action
(defn alpha-policy
  "Maximise a broader view of the current state."
  [player state]

  (let [position (fn [curr-st next-st]
                   (- (points player next-st)
                      (points player curr-st)))]
    (argmax #(position state (apply-action % state))
            (available-actions player state))))

;; The End