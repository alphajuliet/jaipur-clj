;; actions_test.clj

(ns jaipur-clj.actions-test
  (:require [clojure.test :refer [deftest testing is]]
            [jaipur-clj.state :as st]
            [jaipur-clj.actions :as act]
            [jaipur-clj.hash-calc :as h]))

(deftest actions-tests
  (testing "init-game"
    (let [s0 (act/init-game 0)]
      (is (= 40 (h/hash-sum (:deck s0))))))

  (testing "take-card"
    (let [s0 (act/init-game 0)
          s1 (assoc-in s0 [:hand :a :spice] 7)
          s2 (act/take-card :leather :a s0)]
      (is (string? (act/take-card-invalid? :spice :a s1)))
      (is (= 1 (get-in s2 [:hand :a :leather])))))

  (testing "sell-cards"
    (let [s0 (act/init-game 0)
          s1 (assoc-in s0 [:tokens :spice] [1])
          s2 (act/sell-cards :spice :a s1)
          s3 (act/sell-cards :spice :b s0)]
      (is (> 11 (get-in s3 [:points :b])))
      (is (= true (string? (act/sell-cards-invalid? :diamond :b s0))))
      (is (= [] (get-in s2 [:tokens :spice])))))

  (testing "exchange-cards"
    (let [s0 (act/init-game 0)
          p {:camel 1 :leather 1}
          m {:gold 1 :spice 1}]
      (is (string? (act/exchange-cards-invalid? p m :a s0)))))

  (testing "end of game"
    (let [s0 (act/init-game 0)
          s1 (act/deal-cards [:market] 55 s0)]
      (is (= false (act/end-of-game? s0)))
      (is (= true (act/end-of-game? s1))))))

;; The End