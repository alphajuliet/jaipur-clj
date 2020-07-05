;; policy_test.clj

(ns jaipur-clj.policy-test
  (:require [clojure.test :refer [deftest testing is]]
            [jaipur-clj.state :as st]
            [jaipur-clj.actions :as act]
            [jaipur-clj.game :as g]
            [jaipur-clj.policy :as pol]))

(deftest policy-tests
  (testing "Utilities"
    (let [h0 {:a 1 :b 2 :c 3}
          l0 (range 10)]
      (is (= 6 (count (g/key-combinations h0 3))))
      (is (= 5 (count-if even? l0)))
      (is (= :c (pol/argmax-map h0)))
      (is (= -5 (pol/argmax #(Math/abs %) [-5 1 4])))))
  
  #_(testing "Metrics"
    (let [s0 (act/init-game 0)]
      (is (= 125 (pol/beta-metric :a s0))))))

;; The End