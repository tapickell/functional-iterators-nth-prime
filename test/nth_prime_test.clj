(ns nth-prime-test
  (:require [clojure.test :refer [deftest testing is]]
            nth-prime))

(deftest first-prime
  (testing "the first prime is 2"
    (is (= 2 (nth-prime/nth-prime 1)))))

(deftest second-prime
  (testing "the second prime is 3"
    (is (= 3 (nth-prime/nth-prime 2)))))

(deftest sixth-prime
  (testing "the sixth prime is 13"
    (is (= 13 (nth-prime/nth-prime 6)))))

(deftest eighth-prime
  (testing "the eighth prime is 19"
    (is (= 19 (nth-prime/nth-prime 8)))))

(deftest nineth-prime
  (testing "the nineth prime is 23"
    (is (= 23 (nth-prime/nth-prime 9)))))

(deftest tenth-prime
  (testing "the tenth prime is 29"
    (is (= 29 (nth-prime/nth-prime 10)))))

(deftest ten-thousand-first-prime
  (testing "the ten thousand and first prime is 104743"
    (is (= 104743 (nth-prime/nth-prime 10001)))))

(deftest zeroth-prime
  (testing "there is no zeroth prime"
    (is (thrown? IllegalArgumentException (nth-prime/nth-prime 0)))))
