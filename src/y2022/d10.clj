(ns y2022.d10
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/10

;; Generator Logic

(defn parse-command [l]
  (let [split-l (str/split l #" ")]
    (case (first split-l)
      "noop" {:command :noop}
      "addx" {:command :addx :value (parse-long (second split-l))})))

;; Solution Logic

(defn execute-command [cycle x {:keys [command value]}]
  (case command
    :noop [(inc cycle) x]
    :addx [(+ 2)]))

(defn simulate [commands cycles-to-find]
  (loop [cycle 0 x 1 cycles-to-find cycles-to-find found-cycles {} commands commands pending-command nil]
    #_(println "cycle" cycle "x" x "pending" pending-command)
    (let [found-cycles (if (cycles-to-find cycle)
                         (assoc found-cycles cycle (* cycle x))
                         found-cycles)]
      (if (or (empty? cycles-to-find) (empty? commands))
        found-cycles
        (let [[x commands pending-command] (if (and pending-command (zero? pending-command))
                                             [(+ x (:value (first commands))) (rest commands) nil]
                                             [x commands pending-command])]
          (if-let [{:keys [command]} (first commands)]
            (if pending-command
              (recur (inc cycle) x cycles-to-find found-cycles commands (dec pending-command))
              (case command
                :noop (recur (inc cycle) x cycles-to-find found-cycles (rest commands) nil)
                :addx (recur (inc cycle) x cycles-to-find found-cycles commands 1)))
            (recur (inc cycle) x cycles-to-find found-cycles commands pending-command)))))))

(defn simulate-screen [commands]
  (loop [cycle 0 x 1 screen #{} commands commands pending-command nil]
    #_(println "cycle" cycle (mod cycle 40) "x" x "pending" pending-command "draw" (<= (dec x) (mod cycle 40) (inc x)))
    (if (= cycle 241)
      screen
      (let [[x commands pending-command] (if (and pending-command (zero? pending-command))
                                           [(+ x (:value (first commands))) (rest commands) nil]
                                           [x commands pending-command])
            screen (if (<= (dec x) (mod cycle 40) (inc x))
                     (conj screen cycle)
                     screen)]
        (if-let [{:keys [command]} (first commands)]
          (if pending-command
            (recur (inc cycle) x screen commands (dec pending-command))
            (case command
              :noop (recur (inc cycle) x screen (rest commands) nil)
              :addx (recur (inc cycle) x screen commands 1)))
          (recur (inc cycle) x screen commands pending-command))))))

(defn print-screen [screen]
  (doseq [y (range 6)]
    (doseq [x (range 40)]
      (if (screen (+ (* 40 y) x))
        (print "#")
        (print " ")))
    (println)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-command)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [cycles (simulate input #{20 60 100 140 180 220})]
    (reduce + (vals cycles))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [screen (simulate-screen input)]
    (print-screen screen)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
