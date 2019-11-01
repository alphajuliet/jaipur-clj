;; state_test.clj

(ns jaipur-clj.state-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.state :refer :all]
            [lentes.core :as l]))

(deftest state-tests
  (testing "state"
    (is (= 7 (count
              (l/focus (l/in [:tokens :cloth]) initial-state))))
    (is (= 55 (h/hash-sum all-cards)))
    (is (= 44 (count-cards-excl-camels all-cards)))))

;; The End