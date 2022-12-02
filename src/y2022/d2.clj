(ns y2022.d2
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/2

(defn gen-score [[them you]]
  (case them
    :A (case you
         :X 4 #_(+ 1 3)
         :Y 8 #_(+ 6 2)
         :Z 3 #_(+ 0 3))
    :B (case you
         :X 1 #_(+ 0 1)
         :Y 5 #_(+ 3 2)
         :Z 9 #_(+ 6 3))
    :C (case you
         :X 7 #_(+ 6 1)
         :Y 2 #_(+ 0 2)
         :Z 6 #_(+ 3 3))))

(defn gen-score2 [[them you]]
  (case them
    :A (case you
         :X 3 #_(+ 0 3)
         :Y 4 #_(+ 3 1)
         :Z 8 #_(+ 6 2))
    :B (case you
         :X 1 #_(+ 0 1)
         :Y 5 #_(+ 3 2)
         :Z 9 #_(+ 6 3))
    :C (case you
         :X 2 #_(+ 0 2)
         :Y 6 #_(+ 3 3)
         :Z 7 #_(+ 6 1))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv #(mapv keyword (str/split % #" ")))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (reduce + (map gen-score input)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (reduce + (map gen-score2 input)))
