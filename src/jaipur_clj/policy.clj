;; policy.clj
;; Game playing policies
;; AndrewJ 2019-09-28

(ns jaipur-clj.policy
  (:require [clojure.java.io :as io]
            [jaipur-clj.actions :as act]
            [jaipur-clj.game :as game]
            [jaipur-clj.state :as st]
            [taoensso.tufte :as tufte :refer [p]]))

;;-------------------------------
;; Logging setup

(def log-file-name "game.txt")
(io/delete-file log-file-name :quiet)

(defn log
  "Print a string to a log file."
  [s]
  (spit log-file-name s :append true)
  (spit log-file-name "\n" :append true))

;; Profiling setup
(tufte/add-basic-println-handler! {})

;;-------------------------------
;; Utility functions

;; random-value :: Hash a b -> b
(def random-value (comp rand-nth vals))

(defn argmax-map
  "Find the key with the maximum value."
  [m]
  {:pre [(map? m)]}
  (key (apply max-key val m)))

(defn argmax
  "Return the value x in xs that maximises (f x)."
  [f xs]
  (apply max-key f xs))

(defn argmin
  "Return the value x in xs that minimises (f x)."
  [f xs]
  (apply min-key f xs))

;; From https://stackoverflow.com/questions/1601321/idiomatic-mode-function-in-clojure
(defn- tally-map
  "Create a map where the keys are all of the unique elements in the input
   sequence and the values represent the number of times those elements
   occur. Note that the keys may not be formatted as conventional Clojure
   keys, i.e. a colon preceding a symbol."
  [aseq]
  (apply merge-with + (map (fn [x] {x 1}) aseq)))

(defn- dot-product
  "Dot product of two vectors."
  [v1 v2]
  {:pre [(= (count v1) (count v2))]}
  (reduce + (map * v1 v2)))

;;-------------------------------
;; type Policy = Player -> State -> Action
;; apply-policy :: Policy -> Player -> State -> State
(defn apply-policy
  "Apply a given policy function to generate the next state."
  [policy player st]
  (let [action (p ::policy (policy player st))
        new-state (p ::apply-action (game/apply-action action st))]
    (log action)
    (log (st/print-state player new-state))
    new-state))

;;-------------------------------
;; play-game :: Policy -> State -> State
(defn play-game
  "Play a game with a policy function for each player. 
   Limit the number of turns per player to `max-turns` with default 100."

  ;; Limit to 100 iterations if none specified.
  ([policy-a policy-b initial-state]
   (play-game policy-a policy-b initial-state 100))

  ([policy-a policy-b initial-state max-turns]

   (log (str policy-a))
   (log (str policy-b))
   (log "---- Initial state ----")
   (log (st/print-state :a initial-state))
   (log (st/print-state :b initial-state))

  ;; Iterate through the actions for each player to generate a final state
   (reduce
    (fn [state i]
      (if (game/end-of-game? state)
        (reduced
         (let [final-state (act/apply-end-bonus state)]
           (log "---- Final state ----")
           (log (st/print-state :a final-state))
           (log (st/print-state :b final-state))
           final-state))
        ;else
        (do
          (log (format "---- Iteration %d: ----" i))
          (->> state
               (apply-policy policy-a :a)
               (apply-policy policy-b :b)))))
    initial-state
    (range max-turns))))

(defn- identify-winner
  "Identify the winner"
  [st]
  (argmax-map (:points st)))

(defn play-n-games
  "Play n games using the same policies and initial state, and aggregate the wins."
  [n policy-a policy-b initial-state]
  (tally-map
   (reduce (fn [s _]
             (->> initial-state
                  (play-game policy-a policy-b)
                  (identify-winner)
                  (conj s)))
           []
           (range n))))

;;-------------------------------
;; Policies

;; random-policy :: Player -> State -> Action
(defn random-policy
  "Choose a random action from the ones available."
  [player state]
  (->> (game/available-actions player state)
       (rand-nth)))

(defn- score-points
  "Score the number of points."
  [player action state]
  (let [next-state (game/apply-action action state)]
    (get-in next-state [:points player])))

;; greedy-policy :: Player -> State -> Action
(defn greedy-policy
  "Choose the available action that maximises the points in the target states.
   If none, then pick a random one."
  [player state]
  (argmax #(score-points player % state)
          (shuffle (game/available-actions player state))))

;; score-a :: Player -> Action -> State -> Integer
(defn- score-points-delta
  "Measure the points difference between two states."
  [player action state]
  (let [next-state (game/apply-action action state)]
    (- (get-in next-state [:points player])
       (get-in state [:points player]))))

;; alpha-policy :: Player -> State -> Action
(defn alpha-policy
  "Maximise delta of points between current and next state."
  [player state]
  (argmax #(score-points-delta player % state)
          (game/available-actions player state)))

(defn- score-dotp
  "Calculate the dot product of each available token against each hand card type."
  [player action state]
  (let [next-state (game/apply-action action state)]
    (dot-product (st/hand-values player next-state)
                 (st/token-values next-state))))

;; gamma-policy :: Player -> State -> Action
(defn gamma-policy
  "Maximise the 'value' of cards in the hand against the tokens remaining."
  [player state]
  (argmax #(score-dotp player % state)
          (game/available-actions player state)))

(defn- score-dotp-points
  "Weighted sum of dot product, number of camels, and points."
  [player action state]
  (let [next-state (game/apply-action action state)]
    (+ (dot-product (st/hand-values player next-state)
                    (st/token-values next-state))
       (* 2 (get-in next-state [:hand player :camel]))
       (* 5 (get-in next-state [:points player])))))

(defn delta-policy
  "Maximise the 'value' of cards in the hand against the tokens remaining,
  plus getting more points."
  [player state]
  (argmax #(score-dotp-points player % state)
          (shuffle (game/available-actions player state))))

(defn- score-epsilon
  "As for `score-dotp-points` but using mean token value."
  [player action state]
  (let [next-state (game/apply-action action state)]
    (+ (* 2 (dot-product (butlast (st/hand-values player next-state))
                         (st/mean-token-values next-state)))
       (* 2 (get-in next-state [:hand player :camel]))
       (* 5 (get-in next-state [:points player])))))

(defn epsilon-policy
  "Maximise the 'value' of cards in the hand against the tokens remaining,
  plus getting more points."
  [player state]
  (argmax #(score-epsilon player % state)
          (shuffle (game/available-actions player state))))


;; The End
