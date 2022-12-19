(ns y2022.d19
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/19

;; Generator Logic

(defn parse-blueprint [b]
  (let [chunks (str/split b #"[: .]+")
        grab (fn [i] (-> chunks (nth i) parse-long))
        blueprint-number (grab 1)
        ore [(grab 6) 0 0 0]
        clay [(grab 12) 0 0 0]
        obsidian [(grab 18) (grab 21) 0 0]
        geode [(grab 27) 0 (grab 30) 0]]
    {:id blueprint-number
     :recipes [ore clay obsidian geode]}))

;; Solution Logic

(defn do-simulation
  [recipes state max-time max-robots max-geodes]
  (loop [branched? false bot-types (range 4)]
    (if-let [bot-type (first bot-types)]
      (if (= (nth max-robots bot-type) (get-in state [:robots bot-type]))
        (recur branched? (rest bot-types))
        (let [recipe (nth recipes bot-type)
              wait-time (->> (range 4)
                             (keep (fn [mineral]
                                     (cond
                                       (zero? (nth recipe mineral)) nil
                                       (<= (nth recipe mineral) (get-in state [:minerals mineral])) 0
                                       (zero? (get-in state [:robots mineral])) (inc max-time)
                                       :else (quot (dec (+ (- (nth recipe mineral)
                                                              (get-in state [:minerals mineral]))
                                                           (get-in state [:robots mineral])))
                                                   (get-in state [:robots mineral])))))
                             (reduce max))
              time-finished (+ 1 (:time state) wait-time)]
          (if (>= time-finished max-time)
            (recur branched? (rest bot-types))
            (let [new-ore (mapv #(- (+ (* (inc wait-time)
                                          (get-in state [:robots %]))
                                       (get-in state [:minerals %]))
                                    (recipe %))
                                (range 4))
                  new-robots (update (:robots state) bot-type inc)
                  remaining-time (- max-time time-finished)
                  only-geodes (+ (nth new-ore 3)
                                 (* remaining-time (nth new-robots 3))
                                 (quot (* (dec remaining-time)
                                          remaining-time)
                                       2))]
              (if (< only-geodes @max-geodes)
                (recur branched? (rest bot-types))
                (do
                  (do-simulation recipes
                                 {:minerals new-ore
                                  :robots new-robots
                                  :time time-finished}
                                 max-time
                                 max-robots
                                 max-geodes)
                  (recur true (rest bot-types))))))))
      (when-not branched?
        (swap! max-geodes max (+ (get-in state [:minerals 3])
                                 (* (get-in state [:robots 3])
                                    (- max-time (:time state)))))))))

(defn run-simulation [blueprint max-time]
  (let [recipes (:recipes blueprint)
        max-robots (conj (mapv #(reduce max (map (fn [r] (nth r %)) recipes)) (range 3)) Integer/MAX_VALUE)
        max-geodes (atom 0)
        state {:minerals [0 0 0 0]
               :robots [1 0 0 0]
               :time 0}]
    (do-simulation recipes state max-time max-robots max-geodes)
    @max-geodes))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-blueprint)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       (map (fn [blueprint]
              (* (:id blueprint) (run-simulation blueprint 24))))
       (reduce +)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       (take 3)
       (map #(run-simulation % 32))
       (reduce *)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
