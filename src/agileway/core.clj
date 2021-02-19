(ns agileway.core
  (:require [clojure.math.numeric-tower :as math]))
(require '[clojure.core.match :refer [match]])



(defn evaluate  [params expr]
  (cond (integer? expr) expr
        (float? expr) expr
        (symbol? expr) ((keyword expr) params)
        :else (match [expr]
                [(['* & rest1] :seq)] (apply * (map (fn [x] (evaluate params x)) rest1))
                [(['power & rest1] :seq)] (apply math/expt (map (fn [x] (evaluate params x)) rest1))
                [(['abs & rest1] :seq)] (apply math/abs (map (fn [x] (evaluate params x)) rest1))
                [(['+ & rest1] :seq)] (apply + (map (fn [x] (evaluate params x)) rest1))
                [(['/ & rest1] :seq)] (apply / (map (fn [x] (evaluate params x)) rest1))
                [(['- & rest1] :seq)] (apply - (map (fn [x] (evaluate params x)) rest1)))))

(evaluate {} '(* 0 0 (+ 1 1 (- 2 5))))
(evaluate {} '(abs -2))
(evaluate {} '(+ 1 1 (abs -2)))
(evaluate {} '(+ 1 1 (power 2 5)))
(evaluate {:x 10} '(* x x))

(evaluate {:x 10 :y 2} '(* x (+ 2 y)))

(defn helper [params]
  ;; (println params)
  (cond
    (symbol? params)
    params
    (= 1 (count params))
    params
    (and (= 1 (count params)) (not-empty (apply clojure.set/intersection
                                                (map set [params  ['+ '- '/ '*]]))))
    nil
    (= 2 (count params))
    (second params)
    (and (= 3 (count params)) (some #{'0} params))
    (do
      (println (filter #(not= '0 %) params))
      (helper (filter #(not= '0 %) params)))
    :else
    ;; params))
    (let [filtered (filter #(not= nil %) params)]
      (if (= (count params) (count filtered))
        params
        (helper filtered)))))

(defn optimize  [expr]
  (cond (integer? expr) expr
        (symbol? expr) expr
        (float? expr) expr
        (= expr '0) nil
        :else
        (match expr
          (['+ & r] :seq)
          (cond
            (every? #{0} r)
            0
            (some #{0} r)
            (helper (cons '+ (filter #(not= 0 %) (map (fn [x] (optimize x)) (rest expr)))))
            :else (helper (cons '+ (map (fn [x] (optimize x)) (rest expr)))))
          (['- & r] :seq)
          (cond
            (every? #{0} r)
            0
            (some #{0} r)
            (helper (cons '- (filter #(not= 0 %) (map (fn [x] (optimize x)) (rest expr)))))
            :else (helper (cons '- (map (fn [x] (optimize x)) (rest expr)))))
          (['* & r] :seq)
          (cond (some #{1} r)
                (helper  (cons '* (filter #(not= 1 %) (map (fn [x] (optimize x)) (rest  expr)))))
                (some #{0} r)
                0
                :else (helper (cons '* (map (fn [x] (optimize x)) (rest expr)))))
          (['/ & r] :seq)
          (cond (some #{1} r)
                (helper  (cons '/ (filter #(not= 1 %) (map (fn [x] (optimize x)) (rest  expr)))))
                (some #{0} r)
                0
                :else (helper (cons '/ (map (fn [x] (optimize x)) (rest expr))))))))

(optimize '(+ 11 0))
(optimize '(* 1 0))
(optimize '(+ 10 (* x 11)))
(optimize '(* 10 (* x 0)))

(optimize '(+ x (+ 0 0) 0 (/ 0 0)))
(optimize '(* x (+ 0 0) 0 (/ 0 0)))
(optimize '(* x (* 0 0) 0 (* 0 0)))



(defn javascript [params]
  (match params
    (['+ & r] :seq)
    (str "(" (clojure.string/join " + " (map #(javascript %) r)) ")")
    (['* & r] :seq)
    (str "(" (clojure.string/join " * " (map #(javascript %) r)) ")")
    (['- & r] :seq)
    (str "(" (clojure.string/join " - " (map #(javascript %) r)) ")")
    (['/ & r] :seq)
    (str "(" (clojure.string/join " / " (map #(javascript %) r)) ")")
    :else params))
(defn javascript-names [params]
  (remove #(re-find  #"[\d+|\+|\*|\-|\\]" %) (map str (set (flatten params)))))

(javascript '(+ 1 (* x x)))
(javascript-names '(+ 1 (* x y)))
(defn ->javascript [name params]
  (str "function " name "(" (clojure.string/join ", " (javascript-names params)) ") { return " (javascript params) "; }"))


(->javascript "example" '(+ 1 (* x y)))