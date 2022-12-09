(ns y2022.d9
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/9

;; Generator Logic

(defn parse-line [l]
  (let [[direction distance] (str/split l #" ")]
    [(keyword direction) (parse-long distance)]))

;; Solution Logic

(defn move-head [[hx hy] direction]
  (case direction
    :U [hx (inc hy)]
    :D [hx (dec hy)]
    :R [(inc hx) hy]
    :L [(dec hx) hy]))

(defn move-tail [[hx hy] [tx ty]]
  (if (or (> (abs (- hx tx)) 1) (> (abs (- hy ty)) 1))
    [(cond (pos? (- hx tx))
           (inc tx)
           (neg? (- hx tx))
           (dec tx)
           :else
           tx)
     (cond (pos? (- hy ty))
           (inc ty)
           (neg? (- hy ty))
           (dec ty)
           :else
           ty)]
    [tx ty]))

(defn compute-move [visited head-location tail-location direction distance]
  (if (zero? distance)
    [visited head-location tail-location]
    (let [new-head-location (move-head head-location direction)
          new-tail-location (move-tail new-head-location tail-location)]
      (compute-move (conj visited new-tail-location)
                    new-head-location
                    new-tail-location
                    direction
                    (dec distance)))))

(defn compute-move2 [visited head-location tails direction distance]
  (if (zero? distance)
    [visited head-location tails]
    (let [new-head-location (move-head head-location direction)
          new-tails (rest (reductions move-tail new-head-location tails))]
      (compute-move2 (conj visited (last new-tails))
                     new-head-location
                     new-tails
                     direction
                     (dec distance)))))

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
  (loop [commands input head [0 0] tail [0 0] visited #{}]
    (if-let [[direction distance] (first commands)]
      (let [[visited head tail] (compute-move visited
                                              head
                                              tail
                                              direction
                                              distance)]
        (recur (rest commands) head tail visited))
      (count visited))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (loop [commands input head [0 0] tails (repeat 9 [0 0]) visited #{}]
    (if-let [[direction distance] (first commands)]
      (let [[visited head tails] (compute-move2 visited
                                                head
                                                tails
                                                direction
                                                distance)]
        (recur (rest commands) head tails visited))
      (count visited))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-input
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(deftest test-part-2
  (let [input (generator sample-input)]
    (t/is (= 36 (solve-part-2 input)))))
