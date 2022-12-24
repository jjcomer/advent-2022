(ns y2022.d24
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]
            [clojure.data.priority-map :as p]))

;; PROBLEM LINK https://adventofcode.com/2022/day/24

;; Generator Logic

(defn find-blizzards [grid]
  (for [row (range (count grid))
        col (range (count (first grid)))
        :when (not (#{\. \#} (get-in grid [row col] \#)))]
    [row col (get-in grid [row col])]))

(defn move-blizzards [grid blizzards]
  (let [max-row (count grid)
        max-col (count (first grid))]
    (map (fn [[row col dir]]
           (case dir
             \< [row (if (zero? (dec col)) (- max-col 2) (dec col)) dir]
             \> [row (if (= (dec max-col) (inc col)) 1 (inc col)) dir]
             \^ [(if (zero? (dec row)) (- max-row 2) (dec row)) col dir]
             \v [(if (= (dec max-row) (inc row)) 1 (inc row)) col dir]))
         blizzards)))

(defn set-blizzards [grid blizzards]
  (reduce (fn [grid [row col _]]
            (assoc-in grid [row col] \b))
          grid
          blizzards))

(defn parse-maps [grid]
  (let [blizzards (find-blizzards grid)
        clean-grid (mapv #(mapv (fn [c] (if (#{\. \#} c) c \.)) %) grid)]
    (loop [grids [(set-blizzards clean-grid blizzards)] blizzards blizzards]
      (let [new-blizzards (move-blizzards grid blizzards)
            new-grid (set-blizzards clean-grid new-blizzards)]
        (if (= new-grid (first grids))
          grids
          (recur (conj grids new-grid) new-blizzards))))))

;; Solution Logic

(defn next-moves [grid [row col]]
  (filter (fn [[row col]]
            (= \. (get-in grid [row col] \#)))
          [[row col]
           [(dec row) col]
           [(inc row) col]
           [row (dec col)]
           [row (inc col)]]))

(defn get-grid [grids time]
  (nth grids (mod time (count grids))))

(defn score [time [prow pcol] [grow gcol]]
  (let [distance (+ (abs (- grow prow))
                    (abs (- gcol pcol)))]
    (+ time distance)))

(defn simulate [grids initial-time inital-position goal-position]
  (loop [queue (p/priority-map [initial-time inital-position] (score initial-time inital-position goal-position))
         seen #{}]
    (if-let [[[current-time current-position :as state] _] (peek queue)]
      (if (= goal-position current-position)
        state
        (let [moves (map vector
                         (repeat (inc current-time))
                         (remove seen (next-moves (get-grid grids (inc current-time)) current-position)))]
          (recur (reduce (fn [queue [time position :as key]]
                           (assoc queue
                                  key
                                  (score time position goal-position)))
                         (pop queue)
                         moves)
                 (into seen moves))))
      (throw (ex-info "COULDN'T FIND PATH" {})))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv #(into [] %))
       parse-maps))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [grid (first input)
        entry [0 1]
        exit [(dec (count grid))
              (- (count (first grid)) 2)]]
    (simulate input 0 entry exit)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [grid (first input)
        entry [0 1]
        exit [(dec (count grid))
              (- (count (first grid)) 2)]
        [one-time one-position] (simulate input 0 entry exit)
        [two-time two-position] (simulate input one-time one-position entry)]
    (simulate input two-time two-position exit)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
