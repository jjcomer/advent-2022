(ns y2022.d11
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/11

;; Generator Logic

(defn parse-operation [operation]
  (let [[p1 operation p2] (str/split (subs operation 19) #" ")]
    {:operation (keyword operation)
     :p1 (if (= "old" p1)
           :old
           (parse-long p1))
     :p2 (if (= "old" p2)
           :old
           (parse-long p2))}))

(defn parse-monkey [monkey]
  (let [[raw-monkey raw-start raw-operation raw-test raw-true raw-false] (str/split-lines monkey)
        monkey-num (parse-long (subs raw-monkey 7 (- (count raw-monkey) 1)))
        start-items (mapv parse-long (str/split (subs raw-start 18) #", "))
        operation (parse-operation raw-operation)
        test (parse-long (subs raw-test 21))
        true-monkey (parse-long (subs raw-true 29))
        false-monkey (parse-long (subs raw-false 30))]
    {:monkey monkey-num
     :items start-items
     :operation operation
     :test test
     :true-monkey true-monkey
     :false-monkey false-monkey}))

;; Solution Logic

(defn perform-operation [{:keys [p1 p2 operation]} item]
  (let [operation (case operation :+ + :* *)]
    (operation (if (= p1 :old) item p1) (if (= p2 :old) item p2))))

(defn perform-turn [monkeys monkey congruence]
  (let [{:keys [operation test true-monkey false-monkey]} monkey]
    (loop [monkeys monkeys items (:items monkey)]
      (if-let [item (first items)]
        (let [new-item (if congruence
                         (mod (perform-operation operation item) congruence)
                         (quot (perform-operation operation item) 3))
              target-monkey (if (zero? (mod new-item test))
                              true-monkey false-monkey)]
          (recur (update-in monkeys [target-monkey :items] conj new-item) (rest items)))
        (assoc-in monkeys [(:monkey monkey) :items] [])))))

(defn initial-holdings [monkeys]
  (reduce (fn [acc k]
            (assoc acc k 0)) {} (range (count monkeys))))

(defn gen-congruence [monkeys]
  (reduce * (map :test monkeys)))

(defn perform-rounds [monkeys n congruence]
  (loop [n (range n) monkeys monkeys holdings (initial-holdings monkeys)]
    (if (first n)
      (let [[new-holdings new-monkeys] (reduce (fn [[holdings monkeys] monkey]
                                                 (let [monkey (nth monkeys monkey)]
                                                   [(update holdings (:monkey monkey) + (count (:items monkey)))
                                                    (perform-turn monkeys monkey congruence)]))
                                               [holdings monkeys]
                                               (range (count monkeys)))]
        (recur (rest n) new-monkeys new-holdings))
      [monkeys holdings])))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (mapv parse-monkey (-> input
                         str/trim
                         (str/split #"\n\n"))))

(defn monkey-business [holdings]
  (->> holdings
       vals
       sort
       reverse
       (take 2)
       (reduce *)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [[_ holdings] (perform-rounds input 20 nil)]
    (monkey-business holdings)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [[_ holdings] (perform-rounds input 10000 (gen-congruence input))]
    (monkey-business holdings)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-input
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(deftest part1-test
  (let [input (generator sample-input)]
    (println input)
    (t/is (= 10605 (solve-part-1 input)))))

(deftest part2-test
  (let [input (generator sample-input)]
    (println input)
    (t/is (= 2713310158 (solve-part-2 input)))))

