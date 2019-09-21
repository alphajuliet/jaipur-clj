;; core_test.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.core-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.core :refer :all]
            [lentes.core :as l]
            [clojure.test :refer :all]))

(deftest sanity-check
  (is (= 5 (+ 2 3))))

; State
(deftest state-tests
  (is (= 7 (count 
            (l/focus (l/in [:tokens :cloth]) 
                     initial-state)))))

; The End