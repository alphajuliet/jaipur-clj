;; policy.clj
;; Game playing policies
;; AndrewJ 2019-09-28

(ns jaipur-clj.core
  (:require [jaipur-clj.hash-calc :as h]))

;-------------------------------
; Utility functions

; random-value :: Hash a b -> b
(def random-value (comp rand-nth vals))

;-------------------------------
; type Policy = Player -> State -> Action
; apply-policy :: Policy -> Player -> State -> State
(defn apply-policy
  "Apply a given policy function to generate the next state."
  [policy plyr st]
  (let [action (policy plyr st)]
    (pp/pprint action)
    #_(ppst st)
    (apply-action action st)))

;-------------------------------
; Random policy

; policy-random :: Player -> State -> Action
(defn policy-random
  "Choose a random action from the ones available."
  [player state]
  (->> (available-actions player state)
       (group-by first)
       (random-value)
       (rand-nth)))

;-------------------------------
; play-game :: Policy -> State -> State
(defn play-game
  "Play a game with a policy function for both players."
  ([policy initial-state]
   (play-game policy initial-state 100))

  ([policy initial-state max-iter]

  ; Iterate through the actions for each player to generate a final state
   (reduce
    (fn [state i]
      (cond (end-of-game? state) (reduced (apply-end-bonus state))
            :else (do
                    (println (format "Iteration %d:" i))
                    (->> state
                         (apply-policy policy :a)
                         (apply-policy policy :b)))))
    initial-state
    (range max-iter))))


#_(for [iteration (range max-iter)
        :while (< iteration max-iter)
        :while (not (end-of-game? st))]
    (->> st
         (apply-policy policy :a)
         (apply-policy policy :b)))
     ; Add the bonus points for the more camels





;; The End
