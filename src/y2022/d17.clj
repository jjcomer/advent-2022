(ns y2022.d17
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/17

;; Generator Logic

;; Solution Logic

(def rocks [[[0 0] [1 0] [2 0] [3 0]]
            [[1 0] [0 1] [2 1] [1 2]]
            [[0 0] [1 0] [2 0] [2 1] [2 2]]
            [[0 0] [0 1] [0 2] [0 3]]
            [[0 0] [1 0] [0 1] [1 1]]])

(defn add [[ra ia] [rb ib]]
  [(+ ra rb) (+ ia ib)])

(defn empty-spot [tower [r i :as point]]
  (and (<= 0 r 6)
       (pos? i)
       (not (contains? tower point))))

(defn tower-height [tower]
  (reduce max (map second tower)))

(defn drop-rocks [max-cycles jets]
  (loop [cycles (range max-cycles) next-rock 0 next-jet 0 tower #{[-1 0]} cache {}]
    (let [height (tower-height tower)
          start-point [2 (+ 4 height)]]
      (if-let [cycle (first cycles)]
        (let [cache-check (get cache [next-rock next-jet])
              cache (if-not cache-check
                      (assoc cache [next-rock next-jet] [cycle height])
                      cache)]
          (if (and cache-check (zero? (mod (- max-cycles cycle) (- (first cache-check) cycle))))
            [:found-cycle (+ height (* (- (second cache-check) height) (quot (- max-cycles cycle) (- (first cache-check) cycle))))]
            (let [current-rock (nth rocks next-rock)
                  [rock-delta next-jet] (loop [point start-point next-jet next-jet]
                                          (let [jet (if (= :left (nth jets next-jet)) [-1 0] [1 0])
                                                next-jet (mod (inc next-jet) (count jets))
                                                point (if (every? #(empty-spot tower (add point (add jet %))) current-rock)
                                                        (add point jet)
                                                        point)]
                                            (if (every? #(empty-spot tower (add point (add [0 -1] %))) current-rock)
                                              (recur (add point [0 -1]) next-jet)
                                              [point next-jet])))
                  rock (map #(add rock-delta %) current-rock)]
              (recur (rest cycles)
                     (mod (inc next-rock) (count rocks))
                     next-jet
                     (apply conj tower rock)
                     cache))))
        [:end-cycle height]))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       (mapv #(case %
                \< :left
                \> :right))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (drop-rocks 2022 input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (drop-rocks 1000000000000 input))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
