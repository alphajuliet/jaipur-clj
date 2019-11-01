;; actions_test.clj

(ns jaipur-clj.actions-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.state :refer :all]
            [jaipur-clj.actions :refer :all]
            [jaipur-clj.hash-calc :as h]
            [lentes.core :as l]))

(deftest actions-tests
  (testing "init-game"
    (is (= 40 (h/hash-sum (l/focus _deck (init-game))))))

  (testing "take-card"
    (let [s0 (init-game 0)
          s1 (l/put (l/in [:hand :a :spice]) 7 s0)
          s2 (take-card :leather :a s0)]
      (is true (string? (take-card-invalid? :spice :a s1)))
      (is (= 1 (l/focus (l/in [:hand :a :leather]) s2)))))

  (testing "sell-cards"
    (let [s0 (init-game 0)
          s1 (l/put (l/in [:tokens :spice]) [1] s0)
          s2 (sell-cards :spice :a s1)
          s3 (sell-cards :spice :b s0)]
      (is (> 11 (l/focus (comp _points (l/key :b)) s3)))
      (is (= true (string? (sell-cards-invalid? :diamond :b s0))))
      (is (= [] (l/focus (l/in [:tokens :spice]) s2)))))

  (testing "exchange-cards"
    (let [s0 (init-game 0)
          p {:silver 1 :leather 1}
          m {:gold 1 :spice 1}]
      (is (string? (exchange-cards-invalid? p m :a s0)))))

  (testing "end of game"
    (let [s0 (init-game 0)
          s1 (deal-cards _market 55 s0)]
      (is (= false (end-of-game? s0)))
      (is (= true (end-of-game? s1)))))

  (deftest game-tests
    (testing "Utilities"
      (let [h0 {:a 1 :b 2 :c 3}
            l0 (range 10)]
        (is (= 6 (count (h/key-combinations h0 3))))
        (is (= 5 (count-if even? l0)))
        (is (= :c (argmax-map h0)))
        (is (= -5 (argmax #(Math/abs %) [-5 1 4])))))
    (testing "available-actions"
      (let [s0 (init-game 0)]
        (is (= 2 (count (take-card-options :a s0))))
        (is (= 2 (count (sell-cards-options :a s0))))
        (is (= 3 (count (exchange-cards-options :a s0))))
        (is (= 7 (count (available-actions :a s0))))))))

;; The End