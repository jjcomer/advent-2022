(ns y2022.d23
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2022/day/23

;; Generator Logic

;; Solution Logic

(defn north? [elves [y x]]
  (when (not-any? elves [[(dec y) (dec x)] [(dec y) x] [(dec y) (inc x)]])
    :north))

(defn south? [elves [y x]]
  (when (not-any? elves [[(inc y) (inc x)] [(inc y) x] [(inc y) (dec x)]])
    :south))

(defn east? [elves [y x]]
  (when (not-any? elves [[(inc y) (inc x)] [y (inc x)] [(dec y) (inc x)]])
    :east))

(defn west? [elves [y x]]
  (when (not-any? elves [[(inc y) (dec x)] [y (dec x)] [(dec y) (dec x)]])
    :west))


(defn move [direction [y x :as pos]]
  (case direction
    :north [pos [(dec y) x]]
    :south [pos [(inc y) x]]
    :east [pos [y (inc x)]]
    :west [pos [y (dec x)]]))

(defn propose [elves rules elf]
  (let [[r1 r2 r3 r4 :as results] (map #(% elves elf) rules)]
    (cond
      (every? some? results) [elf elf]
      r1 (move r1 elf)
      r2 (move r2 elf)
      r3 (move r3 elf)
      r4 (move r4 elf)
      :else [elf elf])))

(defn do-move [elves rules]
  (let [proposals (map #(propose elves rules %) elves)
        dupes (frequencies (map second proposals))]
    (into #{} (map (fn [[orig dest]]
                     (if (= 1 (dupes dest)) dest orig)))
          proposals)))

(defn find-answer [elves]
  (let [min-y (reduce min (map first elves))
        max-y (reduce max (map first elves))
        min-x (reduce min (map second elves))
        max-x (reduce max (map second elves))]
    (count (for [y (range min-y (inc max-y))
                 x (range min-x (inc max-x))
                 :when (not (contains? elves [y x]))]
             [y x]))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (reduce (fn [elves [y line]]
            (set/union elves
                       (into #{}
                             (comp (filter #(= \# (second %)))
                                   (map (fn [[x _]] [y x])))
                             (map-indexed vector line))))
          #{}
          (->> input
               str/trim
               str/split-lines
               (map-indexed vector))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (loop [cycles 0 elves input rules (cycle [north? south? west? east?])]
    (if (= 10 cycles)
      (find-answer elves)
      (recur (inc cycles)
             (do-move elves (take 4 rules))
             (rest rules)))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (loop [cycles 0 elves input rules (cycle [north? south? west? east?])]
    (let [new-elves (do-move elves (take 4 rules))]
      (if (= elves new-elves)
        (inc cycles)
        (recur (inc cycles)
               new-elves
               (rest rules))))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
