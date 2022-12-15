(ns y2022.d15
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/15

;; Generator Logic
(def line-regex #".*x=([-\d]+), y=([-\d]+).*x=([-\d]+), y=([-\d]+)")

(defn parse-line [line]
  (let [[_ sx sy bx by] (re-find line-regex line)]
    [[(parse-long sx) (parse-long sy)]
     [(parse-long bx) (parse-long by)]]))

;; Solution Logic

(defn find-coverage [sensors beacons]
  (let [y 2000000
        ranges (loop [ranges #{} sensors sensors]
                 (if-let [[[sx sy] [bx by]] (first sensors)]
                   (let [distance (+ (abs (- sx bx)) (abs (- sy by)))
                         distance-to-y (abs (- sy y))
                         y-row (abs (- distance distance-to-y))]
                     (if (>= distance distance-to-y)
                       (recur (conj ranges [(- sx y-row)
                                            (+ sx y-row)])
                              (rest sensors))
                       (recur ranges (rest sensors))))
                   ranges))
        covered (reduce (fn [covered [r0 r1]]
                          (into covered (range r0 (inc r1))))
                        #{}
                        ranges)
        beacons-in-y (count (filter #(= y (second %)) beacons))]
    (- (count covered)
       beacons-in-y)))

(defn find-gap [sensors]
  (let [sensors (set (map (fn [[[sx sy] [bx by]]]
                            [[sx sy] [bx by] (+ (abs (- sx bx)) (abs (- sy by)))])
                          sensors))
        width 4000000]
    (loop [targets (range (inc width))]
      (when-let [target (first targets)]
        (let [segments (reduce (fn [segments [[sx sy] _ distance]]
                                 (let [test (- distance (abs (- target sy)))]
                                   (if (>= test 0)
                                     (conj segments [(max 0 (- sx test)), (min width (+ sx test))])
                                     segments)))
                               []
                               sensors)
              segments (sort segments)
              gap (loop [end (second (first segments))
                         segments (rest segments)]
                    (when-let [[new-start new-end] (first segments)]
                      (if (> new-start end)
                        [(dec new-start) target]
                        (recur (max end new-end)
                               (rest segments)))))]
          (if gap
            gap
            (recur (rest targets))))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-line)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [beacons (set (mapv second input))]
    (find-coverage input beacons)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[x y] (find-gap input)]
    (+ y (* x 4000000))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
