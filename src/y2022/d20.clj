(ns y2022.d20
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/20

;; Generator Logic

;; Solution Logic

(defn build-list [nums]
  (let [circ-list (mapv (fn [n] {:v n :next (atom nil) :prev (atom nil)}) nums)]
    (doseq [i (range (dec (count nums)))]
      (reset! (get-in circ-list [i :next]) (circ-list (inc i))))
    (reset! (get-in circ-list [(dec (count nums)) :next]) (circ-list 0))
    (doseq [i (range 1 (count nums))]
      (reset! (get-in circ-list [i :prev]) (circ-list (dec i))))
    (reset! (get-in circ-list [0 :prev]) (circ-list (dec (count nums))))
    circ-list))

(defn lookup [elem n direction]
  (if (zero? n)
    elem
    (let [next (get elem direction)]
      (recur @next (dec n) direction))))

(defn remove-item [elem]
  (let [prev @(:prev elem)
        next @(:next elem)]
    (reset! (:next prev) next)
    (reset! (:prev next) prev)))

(defn insert-after [elem1 elem2]
  (let [next @(:next elem2)]
    (reset! (:next elem2) elem1)
    (reset! (:prev elem1) elem2)
    (reset! (:prev next) elem1)
    (reset! (:next elem1) next)))

(defn insert-before [elem1 elem2]
  (let [prev @(:prev elem2)]
    (reset! (:prev elem2) elem1)
    (reset! (:next elem1) elem2)
    (reset! (:next prev) elem1)
    (reset! (:prev elem1) prev)))

(defn move [elems i]
  (let [elem (nth elems i)
        val (:v elem)
        direction (if (pos? val) :next :prev)
        to-move (mod (abs val) (dec (count elems)))]
    (if (zero? to-move)
      elems
      (do
        (remove-item elem)
        (let [new-position (lookup elem to-move direction)]
          (if (pos? val)
            (insert-after elem new-position)
            (insert-before elem new-position)))
        elems))))

(defn solve [elems]
  (reduce move elems (range (count elems))))

(defn find-v [elems v]
  (some #(when (= (:v %) v) %) elems))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-long)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [mixed (solve (build-list input))
        start (find-v mixed 0)
        one (lookup start 1000 :next)
        two (lookup one 1000 :next)
        three (lookup two 1000 :next)]
    (reduce + (map :v [one two three]))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [mixed (->> input
                   (map #(* 811589153 %))
                   build-list)
        mixed (reduce (fn [m _]
                        (solve m))
                      mixed
                      (range 10))
        start (find-v mixed 0)
        one (lookup start 1000 :next)
        two (lookup one 1000 :next)
        three (lookup two 1000 :next)]
    (reduce + (map :v [one two three]))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question


(def sample
  "1
2
-3
3
-2
0
4")

(deftest sample-input
  (let [input (generator sample)]
    (t/is (= 3 (solve-part-1 input)))))

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
