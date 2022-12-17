(ns y2022.d16
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/16

;; Generator Logic

(def line-regex #"^Valve (\w\w) has flow rate=(\d+); tunnels? leads? to valves? (.*)$")

(defn parse-line [line]
  (let [[_ valve flow neighbours] (re-find line-regex line)
        neighbours (str/split neighbours #", ")]
    {:name valve
     :flow (parse-long flow)
     :neighbours (atom (zipmap neighbours (repeat 1)))}))

(defn build-graph [valves]
  (doseq [[valve-a valve-b] (for [a valves b valves] [a b])]
    (when (and (not= valve-a valve-b) (contains? @(:neighbours valve-b) (:name valve-a)))
      (let [a-neighbours (:neighbours valve-a)
            b-neighbours (:neighbours valve-b)
            current-distance (get @b-neighbours (:name valve-a))]
        (doseq [neighbour (keys @a-neighbours)]
          (when-not (= neighbour (:name valve-b))
            (if-not (contains? @b-neighbours neighbour)
              (swap! b-neighbours assoc neighbour (+ current-distance (get @a-neighbours neighbour)))
              (let [n-distance (get @b-neighbours neighbour)]
                (swap! b-neighbours assoc neighbour (min n-distance (+ current-distance (get @a-neighbours neighbour))))))))
        (when (zero? (:flow valve-a))
          (swap! b-neighbours dissoc (:name valve-a))))))
  (reduce (fn [valves valve]
            (if (or (= (:name valve) "AA") (pos? (:flow valve)))
              (assoc valves (:name valve) (update valve :neighbours deref))
              valves))
          {}
          valves))

;; Solution Logic

(defn in [l x]
  (some #(= x %) l))

(defn traverse [valves current-valve visited time max-time]
  (if (>= time max-time)
    [0 visited]
    (let [{:keys [flow neighbours name]} (get valves current-valve)
          score (* flow (- max-time time))
          new-visited (concat visited [name])
          children (sequence (comp (filter #(not (in visited %)))
                                   (map #(traverse valves
                                                   %
                                                   new-visited
                                                   (+ 1 time (get neighbours %))
                                                   max-time)))
                             (keys neighbours))
          [child-score child-visited] (reduce (fn [c1 c2]
                                                (if (> (first c2) (first c1)) c2 c1))
                                              children)]
      [(+ score child-score) (concat [name] child-visited)])))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (map parse-line)
       build-graph))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [[score _] (traverse input "AA" [] 0 30)]
    score))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]

  (let [[score visited] (traverse input "AA" [] 0 26)
        visited (rest visited)
        adjusted-valves (reduce-kv (fn [acc name valve]
                                     (let [valve (cond-> valve
                                                   (in visited name) (assoc :flow 0)
                                                   true (update :neighbours #(apply dissoc % visited)))]
                                       (assoc acc name valve)))
                                   {}
                                   input)
        [elephant-score _] (traverse adjusted-valves "AA" [] 0 26)]
    (+ score elephant-score)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
