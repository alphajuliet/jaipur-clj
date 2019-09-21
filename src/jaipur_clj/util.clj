;; util.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.util)

(defn random-element [lst]
  (-> lst
      shuffle
      first))

;; The End