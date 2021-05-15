;; rl-wrapper.clj
;; AndrewJ 2021-05-13
;;
;; Create a RL wrapper for Jaipur
;; See https://juliareinforcementlearning.org/blog/how_to_write_a_customized_environment/
;;
;; action_space(env::YourEnv)
;; state(env::YourEnv)
;; state_space(env::YourEnv)
;; reward(env::YourEnv)
;; is_terminated(env::YourEnv)
;; reset!(env::YourEnv)
;; (env::YourEnv)(action)

(ns jaipur-clj.rl-wrapper
  (:require [jaipur-clj.state :as st]
            [jaipur-clj.actions :as act]
            [jaipur-clj.hash-calc :as h]
            [jaipur-clj.game :as game]))

(defn action-space
  "Return the available actions in the given state."
  [state]
  (let [player (:turn state)]
    (game/available-actions player state)))

(defn state
  [state])

(defn state-space
  [state])

(defn reward
  [state])

(defn is-terminated
  [state]
  (game/end-of-game? state))

(defn reset
  [_]
  (game/init-game 0))

(defn apply-action
  [state action]
  (game/apply-action action state))


;; The End
