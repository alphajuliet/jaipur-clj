;; game_test.clj

(ns jaipur-clj.game-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.state :refer :all]
            [jaipur-clj.actions :refer :all]
            [jaipur-clj.game :refer :all]))

(deftest game-tests
  (testing "available-actions"
    (let [s0 (init-game 0)]
      (is (= 2 (count (take-card-options :a s0))))
      (is (= 2 (count (sell-cards-options :a s0))))
      (is (= 3 (count (exchange-cards-options :a s0))))
      (is (= 7 (count (available-actions :a s0)))))))

;; The End