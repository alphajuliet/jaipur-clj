;; actions.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.core
  (:require [lentes.core :as l]
            [jaipur-clj.hash-calc :as h]
            [jaipur-clj.util :refer :all]))

;-------------------------------
; Random card from the deck
; random-card :: State -> Card
(defn random-card [st]
  (->> (l/focus _deck st)
       (h/hash-enumerate)
       (random-element)))


;-------------------------------
; Move n cards from _src to _dest
; If not possible then throw an error
#_(defn move-cards [rsrc _src _dest n st]
  (if (> n (view (>>> _src (_rsrc rsrc)) st))
    (raise-user-error 'move-card
                      (format "failed because insufficient ~a cards are available to move from ~a."
                              rsrc (view _src st)))
      ;else
    (~>> st
         (over (>>> _src (_rsrc rsrc)) (curry flip - n))
         (over (>>> _dest (_rsrc rsrc)) (curry + n)))))

;; The End