;; state_test.clj

(ns jaipur-clj.state-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [jaipur-clj.state :as st]
            [jaipur-clj.hash-calc :as h]))

(deftest state-tests
  (testing "state"
    (is true (s/valid? ::st/state st/initial-state))
    (is (= 7 (count (get-in st/initial-state [:tokens :cloth]))))
    (is (= 55 (h/hash-sum st/all-cards)))
    (is (= 44 (st/count-cards-excl-camels st/all-cards)))))

;; The End
