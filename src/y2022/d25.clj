(ns y2022.d25
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/25

;; Generator Logic

;; Solution Logic

(defn snafu->digit [c]
  (case c
    \0 0
    \1 1
    \2 2
    \- -1
    \= -2))

(defn snafu->number [snafu]
  (reduce (fn [current n]
            (+ (snafu->digit n)
               (* 5 current)))
          0
          snafu))

(defn number->snafu [number]
  (loop [number number snafu []]
    (if (pos? number)
      (let [rem (mod number 5)
            new-number (quot number 5)]
        (case rem
          3 (recur (inc new-number) (conj snafu -2))
          4 (recur (inc new-number) (conj snafu -1))
          (recur new-number (conj snafu rem))))
      (apply str (map #(case %
                         -2 "="
                         -1 "-"
                         %)
                      (reverse snafu))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (-> input
      str/trim
      str/split-lines))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [sum (reduce + (map snafu->number input))]
    (number->snafu sum)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
