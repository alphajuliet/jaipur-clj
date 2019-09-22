;; core_test.clj
;; AndrewJ 2019-09-21

(ns jaipur-clj.core-test
  (:require [clojure.test :refer :all]
            [jaipur-clj.core :refer :all]
            [jaipur-clj.hash-calc :as h]
            [lentes.core :as l]))

; State
(deftest state-tests
  (testing "state"
    (is (= 7 (count
              (l/focus (l/in [:tokens :cloth]) initial-state))))
    (is (= 55 (h/hash-sum all-cards)))
    (is (= 44 (count-cards-excl-camels all-cards)))))

; Actions
(deftest actions-tests
  (testing "init-game"
    (is (= 40 (h/hash-sum (l/focus _deck (init-game))))))

  (testing "take-card"
    (let [s0 (init-game 0)
          s1 (l/put (l/in [:hand :a :spice]) 7 s0)
          s2 (take-card :leather :a s0)]
      (is true (string? (take-card-invalid? :spice :a s1)))
      (is (= 1 (l/focus (l/in [:hand :a :leather]) s2)))))

  (testing "sell-card"
    (let [s0 (init-game 0)
          s1 (l/put (l/in [:tokens :spice]) [1] s0)
          s2 (sell-cards :spice :a s1)]
      (is (= true (string? (sell-cards-invalid? :diamond :b s0))))
      (is (= [] (l/focus (l/in [:tokens :spice]) s2))))))


;; The End