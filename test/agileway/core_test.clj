(ns agileway.core-test
  (:require [clojure.test :refer :all]
            [agileway.core :refer :all]))


(deftest test-evaluate
  (testing "evaluate function"
    (testing "numbers"
      (is (= (evaluate {} '(* 2 5 (+ 1 1 (- 2 5))))  -10)))
      (is (= (evaluate {} '(* 0 0 (+ 1 1 (- 2 5)))) 0)))
    (testing "abs"
      (is (= (evaluate {} '(abs -2)) 2))
      (is (= (evaluate {} '(+ 1 1 (abs -2))) 4)))
    (testing "power"
      (is (= (evaluate {} '(+ 1 1 (power 2 5))) 34))
      (is (= (evaluate {} '(power 2 5)) 32)))
    (testing "with-name"
      (is (= (evaluate {:x 10} '(* x x)) 100))
      (is (= (evaluate {:x 10 :y 2} '(* x (+ 2 y))) 40))))


(deftest test-optimize
  (testing "optimize function"
    (is (= (optimize '(+ 11 0)) 11))
    (is (= (optimize '(+ x 0)) 'x))
    (is (= (optimize '(* 1 0)) '0))
    (is (= (optimize '(/ 1 0)) '0))
    (is (= (optimize '(/ 1 1)) '1))
    (is (= (optimize '(* 1 1)) '1))
    (is (= (optimize '(/ 1 (/ 1 1) 1)) '1))
    (is (= (optimize '(+ 10 (* x 11))) '(+ 10 (* x 11))))
    (is (= (optimize '(+ x (+ 0 0) 0 (/ 0 0))) 'x))
    (is (= (optimize '(* x (+ 0 0) 0 (/ 0 0))) '0))
    (is (= (optimize '(* x (* 0 0) 0 1 (* 0 0))) '0))
    (is (= (optimize '(+ x (* 0 0) 0 1 (* 0 0))) '(+ x 1)))
    (is (= (optimize '(- x (* 0 0) 0 1 (* 0 0))) '(- x 1)))
    (is (= (optimize '(- x (* 1 1) 0 1 (* 0 0))) '(- x 1 1)))
    ))

(deftest test-javascript
  (testing "javascript function"
    (is (= (->javascript "example" '(+ 1 (* x y))) "function example(x, y) { return (1 + (x * y)); }"))
    ))


(deftest test-javascript-names
  (testing "javascript-names function"
    (is (= (vec (javascript-names '(+ 1 (* x y))))  ["x" "y"]))))