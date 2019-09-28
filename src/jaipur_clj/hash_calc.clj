;; hash-calc.clj
;; Calculations over hash maps with numeric values.
;; AndrewJ 2019-09-21

(ns jaipur-clj.hash-calc)

(defn hash-min [h]
  (apply min (vals h)))

(defn hash-sum
  "Sum the values of the hash."
  [h]
  (apply + (vals h)))

(defn hash-union
  "Combine two hashes with a combining function for values with matching keys"
  [f h1 h2]
  (merge-with f h1 h2))

(defn hash-add
  "Add matching values from two hashes."
  [h1 h2]
  (hash-union + h1 h2))

(defn hash-sub
  "Subtract matching values from two hashes."
  [h1 h2]
  (hash-union - h1 h2))

(defn hash-enumerate
  "For each pair, return v copies of k, and concatenate across the hash."
  [h]
  (reduce-kv
   (fn [m k v]
     (into m (repeat v k))) [] h))

(defn hash-collect
  "Collect a list into a hash of counts."
  [lst]
  (into {} (map (fn [[k v]] [k (count v)])
                (group-by identity lst))))

;(defn hash-intersection [f h1 h2]
;         ;#:combine/key [combine/key (λ (k x y) (* x y))]
;         ;h1 h2)
;  (for/hash ([k (in-list (intersection (hash-keys h1) (hash-keys h2)))])
;            (values k (combine/key k (hash-ref h1 k) (hash-ref h2 k)))))

;(define (hash-union
;         #:combine [combine #f]
;         #:combine/key [combine/key
;                        (if combine
;                          (lambda (k x y) (combine x y))
;                          (hash-duplicate-error 'hash-union))]
;         one . rest)
;  (for*/fold ([one one]) ([two (in-list rest)] [(k v) (in-hash two)])
;    (hash-set one k (if (hash-has-key? one k)
;                        (combine/key k (hash-ref one k) v)
;                        v))))

;; The End