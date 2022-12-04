(ns y2022.d4
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/4

;; Generator Logic

(defn parse-assignments [l]
  (let [nums (map parse-long (str/split l #"[-,]"))]
    [[(first nums) (second nums)]
     [(nth nums 2) (nth nums 3)]]))

;; Solution Logic

(defn subset? [[[a1 a2] [b1 b2]]]
  (or (<= a1 b1 b2 a2)
      (<= b1 a1 a2 b2)))

(defn overlap? [[[a1 a2] [b1 b2]]]
  (or (<= a1 b1 a2)
      (<= b1 a1 b2)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-assignments)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       (filter subset?)
       count))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       (filter overlap?)
       count))

