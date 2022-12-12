(ns y2022.d12
  (:require [clojure.test :as t :refer [deftest]]
            [loom.graph :as g]
            [loom.alg :as ga]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/12

;; Generator Logic

(defn traversable? [from to]
  ;;(println (pr-str from) (pr-str to))
  (let [from (case from
               \E (int \z)
               \S (int \a)
               (int from))
        to (case to
             \E (int \z)
             \S (int \a)
             (int to))]
    (<= to (inc from))))

(defn out-of-bounds? [max-y max-x [y x]]
  (or (neg? x) (neg? y) (>= x max-x) (>= y max-y)))

(defn find-neighbours [nodes y x]
  (let [max-x (count (first nodes))
        max-y (count nodes)
        location (get-in nodes [y x])
        deltas (remove #(out-of-bounds? max-y max-x %)
                       (map (fn [[dy dx]] [(+ dy y) (+ dx x)])
                            [[-1 0] [1 0] [0 1] [0 -1]]))]
    ;;(println "x" x "y" y "location" location "deltas" deltas)
    (filter #(traversable? location (get-in nodes %)) deltas)))

(defn build-graph [nodes]
  ;;(println nodes (get-in nodes [0 0]) (get-in nodes [1 1]))
  (reduce (fn [g [y x]]
            (let [neighbours (find-neighbours nodes y x)
                  g-with-nodes (reduce (fn [g [y x]]
                                         (g/add-nodes g [y x (get-in nodes [y x])]))
                                       g
                                       neighbours)
                  this-node [y x (get-in nodes [y x])]]
              ;;(println this-node neighbours)
              (reduce (fn [g [y x]]
                        (let [other-node [y x (get-in nodes [y x])]]
                          (g/add-edges g [this-node other-node 1])))
                      g-with-nodes
                      neighbours)))
          (g/weighted-digraph)
          (for [x (range (count (first nodes)))
                y (range (count nodes))]
            [y x])))
;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv vec)
       build-graph))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [start (first (filter #(= \S (last %)) (g/nodes input)))
        end (first (filter #(= \E (last %)) (g/nodes input)))
        path (ga/shortest-path input start end)]
    (dec (count path))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [starts (filter #(= \a (last %)) (g/nodes input))
        end (first (filter #(= \E (last %)) (g/nodes input)))
        paths (map #(ga/shortest-path input % end) starts)]
    (->> paths
         (filter seq)
         (map count)
         (map dec)
         (reduce min))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(deftest part-1-test
  (let [input (generator sample-input)]
    (t/is (= 31 (solve-part-1 input)))))

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
