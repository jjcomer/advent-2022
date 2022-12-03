(ns y2022.d3
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/3

;; Generator Logic

(defn parse-backpack [pack]
  (let [midpoint (/ (count pack) 2)]
    [(seq (subs pack 0 midpoint))
     (seq (subs pack midpoint))]))

;; Solution Logic

(defn find-common-item [[a b]]
  (first (set/intersection (set a) (set b))))

(defn find-badge-type [backpacks]
  (->> backpacks
       (map #(set (concat (first %) (second %))))
       (reduce set/intersection)
       first))

(defn score-item [x]
  (let [numeric-x (int x)]
    (if (>= numeric-x 97)
      (- numeric-x 96)
      (- numeric-x 38))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-backpack)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       (map find-common-item)
       (map score-item)
       (reduce +)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       (partition 3)
       (map find-badge-type)
       (map score-item)
       (reduce +)))

