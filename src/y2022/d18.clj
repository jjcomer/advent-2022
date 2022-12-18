(ns y2022.d18
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/18

;; Generator Logic

(defn parse-line [line]
  (mapv parse-long (str/split line #",")))

;; Solution Logic

(defn add-vector [[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])

(def deltas [[-1 0 0] [1 0 0] [0 -1 0] [0 1 0] [0 0 -1] [0 0 1]])

(defn find-pairs [cubes]
  (loop [counter 0 seen #{} cubes cubes]
    (if-let [cube (first cubes)]
      (recur
       (+ counter
          (count (filter seen (map #(add-vector cube %) deltas))))
       (conj seen cube)
       (rest cubes))
      counter)))

(defn find-bounds [cubes]
  (let [min-value (reduce min (flatten cubes))
        max-value (reduce max (flatten cubes))]
    [(dec min-value) (inc max-value)]))

(defn get-nbrs [point lower-bound upper-bound]
  (->> deltas
       (map #(add-vector point %))
       (filter #(every? (fn [x] (<= lower-bound x upper-bound)) %))))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn do-dfs [cubes lower-bound upper-bound]
  (let [start-point [lower-bound lower-bound lower-bound]
        cubes (set cubes)]
    (loop [to-visit (conj empty-queue start-point) seen #{start-point}]
      (if-let [current (peek to-visit)]
        (let [nbrs (remove #(or (seen %) (cubes %)) (get-nbrs current lower-bound upper-bound))]
          (recur (-> to-visit
                     pop
                     (into nbrs))
                 (into seen nbrs)))
        seen))))

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
  (- (* 6 (count input)) (* 2 (find-pairs input))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[lower-bound upper-bound] (find-bounds input)
        steam (do-dfs input lower-bound upper-bound)
        nbrs (mapcat #(get-nbrs % lower-bound upper-bound) input)]
    (count (filter #(steam %) nbrs))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
