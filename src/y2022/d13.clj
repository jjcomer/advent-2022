(ns y2022.d13
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/13

;; Generator Logic

(defn parse-pairs [pair]
  (->> pair
       str/split-lines
       (mapv edn/read-string)))

;; Solution Logic

(defn compare-packets [l, r]
  (cond
    (and (number? l) (number? r))
    (compare l r)

    (and (number? l) (sequential? r))
    (compare-packets [l] r)

    (and (number? r) (sequential? l))
    (compare-packets l [r])

    (and (sequential? r) (sequential? l))
    (loop [comps (map compare-packets l r)]
      (condp = (first comps)
        nil (compare-packets (count l) (count r))
        0 (recur (rest comps))
        (first comps)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (-> input
      str/trim
      (str/split #"\n\n")
      (#(mapv parse-pairs %))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [result (->> input
                    (map-indexed (fn [i [l r]]
                                   [(inc i) l r]))
                    (filter (fn [[_ l r]]
                              (= -1 (compare-packets l r))))
                    (map first))]
    (println result)
    (reduce + result)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [packets (conj (mapcat identity input) [[2]] [[6]])
        sorted (sort compare-packets packets)]
    (->> sorted
         (map-indexed vector)
         (filter #(or (= (second %) [[2]]) (= (second %) [[6]])))
         (map first)
         (map inc)
         (reduce *))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def example-data
  "

[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
")

(deftest sample-data-test
  (let [input (generator example-data)]
    (t/is (= 13 (solve-part-1 input)))))

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
