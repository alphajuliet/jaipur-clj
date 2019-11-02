;; policy_test.clj

(ns jaipur-clj.policy-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.state :refer :all]
            [jaipur-clj.actions :refer :all]
            [jaipur-clj.game :refer :all]
            [jaipur-clj.policy :refer :all]))

(deftest policy-tests
  (testing "Utilities"
    (let [h0 {:a 1 :b 2 :c 3}
          l0 (range 10)]
      (is (= 6 (count (key-combinations h0 3))))
      (is (= 5 (count-if even? l0)))
      (is (= :c (argmax-map h0)))
      (is (= -5 (argmax #(Math/abs %) [-5 1 4]))))))

;; The End