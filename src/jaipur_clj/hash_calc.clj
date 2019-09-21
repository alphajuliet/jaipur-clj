;; hash-calc.clj
;; Calculations over hash maps
;; AndrewJ 2019-09-21

(ns jaipur-clj.hash-calc)

(defn hash-sum [h]
  (reduce + (vals h)))

(defn hash-enumerate [h]
  (reduce-kv 
   (fn [m k v]
     (into m (repeat v k))) [] h))

;; The End