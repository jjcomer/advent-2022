(ns y2022.d14
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/14

;; Generator Logic

(defn parse-points [points]
  (map #(let [[x y] (str/split % #",")]
          [(parse-long x) (parse-long y)])
       (str/split points #" -> ")))

(defn make-range [p1 p2]
  (if (<= p1 p2)
    (range p1 (inc p2))
    (range p1 (dec p2) -1)))

(defn generate-lines [cave points]
  (reduce (fn [cave [[x1 y1] [x2 y2]]]
            (reduce (fn [cave point]
                      (assoc cave point :rock))
                    cave
                    (for [x (make-range x1 x2)
                          y (make-range y1 y2)]
                      [x y])))
          cave
          (partition 2 1 points)))

;; Solution Logic

(defn drop-sand [cave max-y part2?]
  (loop [x 500 y 0]
    (cond
      (and (not part2?) (> y max-y))
      :falling

      (and part2? (= y max-y))
      (assoc cave [x max-y] :sand)

      (not (contains? cave [x (inc y)]))
      (recur x (inc y))

      (not (contains? cave [(dec x) (inc y)]))
      (recur (dec x) (inc y))

      (not (contains? cave [(inc x) (inc y)]))
      (recur (inc x) (inc y))

      :else
      (assoc cave [x y] :sand))))

(defn find-bottom [cave]
  (->> cave
       keys
       (map second)
       (reduce max)
       inc))


(defn simulate-sand [cave part2?]
  (let [max-y (find-bottom cave)]
    (loop [cave cave sand-count 0]
      (let [new-cave (drop-sand cave max-y part2?)]
        (cond
          (and part2? (= :sand (get new-cave [500 0])))
          [new-cave (inc sand-count)]

          (= :falling new-cave)
          [new-cave sand-count]

          :else
          (recur new-cave (inc sand-count)))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (map parse-points)
       (reduce generate-lines {})))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [[_ sand-count] (simulate-sand input false)]
    sand-count))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[_ sand-count] (simulate-sand input true)]
    sand-count))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-data
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(deftest part-2-test
  (let [input (generator sample-data)]
    (t/is (= 93 (solve-part-2 input)))))

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
