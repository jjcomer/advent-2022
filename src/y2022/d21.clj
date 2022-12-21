(ns y2022.d21
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/21

;; Generator Logic

(defn parse-monkey [monkey]
  (let [[monkey-name instructions] (str/split monkey #": ")
        instructions (str/split instructions #" ")]
    (if (= 1 (count instructions))
      {:monkey monkey-name
       :type :number
       :value (parse-long (first instructions))}
      (let [[m1 op m2] instructions]
        {:monkey monkey-name
         :type (keyword op)
         :m1 m1
         :m2 m2}))))

;; Solution Logic

(defn solve [monkeys monkey-name cache]
  (if-let [result (@cache monkey-name)]
    result
    (let [{:keys [type value m1 m2]} (monkeys monkey-name)]
      (if (= :number type)
        (do (swap! cache assoc monkey-name value)
            value)
        (let [m1-value (solve monkeys m1 cache)
              m2-value (solve monkeys m2 cache)
              result (if (and (number? m1-value) (number? m2-value))
                       (case type
                         :* (* m1-value m2-value)
                         :- (- m1-value m2-value)
                         :+ (+ m1-value m2-value)
                         :/ (quot m1-value m2-value)
                         := (= m1-value m2-value))
                       `(~type ~m1-value ~m2-value))]
          (swap! cache assoc monkey-name result)
          result)))))

(defn solve-equation [equation goal]
  (let [[op left right] equation]
    (if (number? left)
      (let [new-goal (case op
                       :+ (- goal left)
                       :- (- left goal)
                       :* (quot goal left)
                       :/ (* goal left))]
        (if (= "x" right)
          new-goal
          (recur right new-goal)))
      (let [new-goal (case op
                       :+ (- goal right)
                       :- (+ goal right)
                       :* (quot goal right)
                       :/ (* goal right))]
        (if (= "x" left)
          new-goal
          (recur left new-goal))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-monkey)
       (reduce #(assoc %1 (:monkey %2) %2) {})))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (solve input "root" (atom {})))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [monkeys (-> input
                    (assoc-in ["root" :type] :=)
                    (assoc-in ["humn" :value] "x"))
        [_ equation goal] (solve monkeys "root" (atom {}))]
    (solve-equation equation goal)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
