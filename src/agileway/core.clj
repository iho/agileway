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

(defn optimize  [expr]
  (cond (integer? expr) expr
        (float? expr) expr
        (symbol? expr) expr
        (= 1 (count expr))
        (cond (= (first expr) '/) 1
              (= (first expr) '*) 1)
        (= 2 (count expr))
        (second expr)
        :else
        (match expr
          (['+ & r] :seq)
          (cond
            (every? #{0} r)
            0
            (some #{0} r)
            (optimize (cons '+ (filter #(not= 0 %) (map (fn [x] (optimize x)) (rest expr)))))
            :else   (cons '+ (map (fn [x] (optimize x)) (rest expr))))
          (['- & r] :seq)
          (cond
            (every? #{0} r)
            0
            (some #{0} r)
            (optimize (cons '- (filter #(not= 0 %) (map (fn [x] (optimize x)) (rest expr)))))
            :else  (cons '- (map (fn [x] (optimize x)) (rest expr))))
          (['* & r] :seq)
          (cond (some #{1} r)
                (optimize  (cons '* (filter #(not= 1 %) (map (fn [x] (optimize x)) (rest  expr)))))
                (some #{0} r)
                0
                :else (cons '* (map (fn [x] (optimize x)) (rest expr))))
          (['/ & r] :seq)
          (cond
            (some #{1} r)
            (optimize  (cons '/ (filter #(not= 1 %) (map (fn [x] (optimize x)) (rest  expr)))))
            (some #{0} r)
            0
            :else (optimize (cons '/ (map (fn [x] (optimize x)) (rest expr)))))
          :else expr
          (let [optimized  (optimize expr)]
            (if (= (count expr) (count optimized))
              expr
              optimized)))))


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

(defn ->javascript [name params]
  (str "function " name "(" (clojure.string/join ", " (javascript-names params)) ") { return " (javascript params) "; }"))