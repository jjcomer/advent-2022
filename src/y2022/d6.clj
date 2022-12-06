(ns y2022.d6)

;; PROBLEM LINK https://adventofcode.com/2022/day/6

;; Solution Logic

(defn find-marker [size packet]
  (loop [head 0 tail packet]
    (when-let [to-test (take size tail)]
      (if (= size (count (set to-test)))
        (+ head size)
        (recur (inc head) (rest tail))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (seq input))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (find-marker 4 input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (find-marker 14 input))
