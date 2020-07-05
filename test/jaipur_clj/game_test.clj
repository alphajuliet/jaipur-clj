;; game_test.clj

(ns jaipur-clj.game-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.state :refer :all]
            [jaipur-clj.actions :as act]
            [jaipur-clj.game :as g]))

(deftest game-tests
  (testing "available-actions"
    (let [s0 (act/init-game 0)
          s1 (act/init-game 1)]
      (is (= 2 (count (g/take-card-options :a s0))))
      (is (= 2 (count (g/sell-cards-options :a s0))))
      (is (= 3 (count (g/exchange-cards-options :a s0))))
      (is (= 2 (count (g/exchange-cards-options :a s1))))
      (is (= 7 (count (g/available-actions :a s0)))))))

;; The End