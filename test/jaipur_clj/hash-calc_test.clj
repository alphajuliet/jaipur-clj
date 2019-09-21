;; hash-calc_test.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.hash-calc-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.hash-calc :refer :all]
            [clojure.test :refer :all]))

(def h1 {:a 1 :b 2 :c 3})

(deftest hash-calc-tests
  (is (= 6 (hash-sum h1)))
  (is (= [:a :b :b :c :c :c] (hash-enumerate h1))))

; The End