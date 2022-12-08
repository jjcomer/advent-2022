(ns y2022.d8
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/8

;; Generator Logic

;; Solution Logic

(defn is-visible? [forest max-x max-y x y]
  (if (or (= 0 x) (= 0 y) (= (dec max-x) x) (= (dec max-y) y))
    true
    (let [tree-height (get-in forest [x y])]
      (or (every? #(< (get-in forest [x %]) tree-height) (range 0 y))
          (every? #(< (get-in forest [x %]) tree-height) (range (inc y) max-y))
          (every? #(< (get-in forest [% y]) tree-height) (range 0 x))
          (every? #(< (get-in forest [% y]) tree-height) (range (inc x) max-x))))))

(defn find-all-visible-trees [forest]
  (let [max-x (count forest)
        max-y (count (first forest))
        points (for [x (range max-x)
                     y (range max-y)]
                 [x y])]
    (filter #(is-visible? forest max-x max-y (first %) (second %)) points)))

(defn scenic-score [forest max-x max-y x y]
  (if (or (= 0 x) (= 0 y) (= (dec max-x) x) (= (dec max-y) y))
    0
    (let [tree-height (get-in forest [x y])
          up (take-while #(< (get-in forest [x %]) tree-height) (reverse (range 0 y)))
          down (take-while #(< (get-in forest [x %]) tree-height) (range (inc y) max-y))
          left (take-while #(< (get-in forest [% y]) tree-height) (reverse (range 0 x)))
          right (take-while #(< (get-in forest [% y]) tree-height) (range (inc x) max-x))]
      (* (if (not= 0 (last up)) (inc (count up)) (count up))
         (if (not= (dec max-y) (last down)) (inc (count down)) (count down))
         (if (not= 0 (last left)) (inc (count left)) (count left))
         (if (not= (dec max-x) (last right)) (inc (count right)) (count right))))))

(defn find-most-scenic-tree [forest]
  (let [max-x (count forest)
        max-y (count (first forest))
        points (for [x (range max-x)
                     y (range max-y)]
                 [x y])]
    (reduce max (map #(scenic-score forest max-x max-y (first %) (second %)) points))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv #(mapv (fn [d] (Character/digit d 10)) %))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       find-all-visible-trees
       count))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (find-most-scenic-tree input))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-input
  "30373
25512
65332
33549
35390")

(deftest part-1
  (let [input (generator sample-input)]
    (t/is (= 21 (solve-part-1 input)))))

(deftest part-2
  (let [input (generator sample-input)]
    (t/is (= 8 (solve-part-2 input)))))
