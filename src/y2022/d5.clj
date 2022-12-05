(ns y2022.d5
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/5

;; Generator Logic

(def crate-regex #"\[(.)\]|(    ?)")

(defn parse-boxes [stacks new-crates]
  (let [crates (re-seq crate-regex new-crates)]
    (reduce (fn [stacks [i crate]]
              (if-not (str/blank? crate)
                (let [current-stack (get stacks i [])]
                  (assoc stacks i (conj current-stack crate)))
                stacks))
            stacks
            (map-indexed (fn [i c] [(inc i) (second c)]) crates))))

(def instruction-regex #"move (\d+) from (\d) to (\d)")

(defn parse-instruction [instruction]
  (mapv parse-long (rest (re-find instruction-regex instruction))))

;; Solution Logic

(defn move-crates [stacks instruction]
  (let [[n from to] instruction
        from-crates (get stacks from)
        to-crates (get stacks to)
        crates-to-move (reverse (take n from-crates))]
    (assoc stacks
           from (drop n from-crates)
           to (concat crates-to-move to-crates))))

(defn move-crates2 [stacks instruction]
  (let [[n from to] instruction
        from-crates (get stacks from)
        to-crates (get stacks to)
        crates-to-move (take n from-crates)]
    (assoc stacks
           from (drop n from-crates)
           to (concat crates-to-move to-crates))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[crates instructions] (str/split input #"\n\n")
        crates (->> crates
                    str/split-lines
                    butlast
                    (reduce parse-boxes (sorted-map)))
        instructions (->> instructions
                          str/trim
                          str/split-lines
                          (mapv parse-instruction))]
    [crates instructions]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [[crates instructions] input
        final-crates (reduce move-crates crates instructions)]
    (apply str (map first (vals final-crates)))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[crates instructions] input
        final-crates (reduce move-crates2 crates instructions)]
    (apply str (map first (vals final-crates)))))

