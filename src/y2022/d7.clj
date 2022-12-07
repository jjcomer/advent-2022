(ns y2022.d7
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2022/day/7


;; Generator Logic

(defn parse-line [line]
  (let [command (str/split line #" ")]
    (case (first command)
      "$" (case (second command)
            "ls" {:type :ls}
            "cd" {:type :cd :name (nth command 2)})
      "dir" {:type :dir :name (second command)}
      {:type :file :name (second command) :size (parse-long (first command))})))

;; Solution Logic

(defn execute-command [filesystem pwd command]
  (case (:type command)
    :ls [filesystem pwd]
    :cd (case (:name command)
          ".." [filesystem (pop pwd)]
          "/" [filesystem ["/"]]
          [filesystem (conj pwd (:name command))])
    :dir [filesystem pwd]
    :file [(assoc-in filesystem (conj pwd :files (:name command)) (:size command)) pwd]))

(defn build-tree [commands]
  (loop [filesystem {} pwd [] commands commands]
    (if-let [command (first commands)]
      (let [[new-filesystem new-pwd] (execute-command filesystem pwd command)]
        (recur new-filesystem new-pwd (rest commands)))
      filesystem)))

(defn directory-sizes [filesystem path]
  (let [local-size (reduce + (vals (get filesystem :files)))
        child-dirs (reduce-kv (fn [acc dir-name filesystem]
                                (if (= :files dir-name)
                                  acc
                                  (merge acc (directory-sizes filesystem (conj path dir-name)))))
                              (sorted-map)
                              filesystem)
        child-sizes (reduce + (vals (select-keys child-dirs (keep #(when-not (= :files %)
                                                                     (conj path %))
                                                                  (keys filesystem)))))]
    (assoc child-dirs path (+ local-size child-sizes))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/trim
       str/split-lines
       (mapv parse-line)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [filesystem (build-tree input)
        directory (directory-sizes (get filesystem "/") ["/"])]
    (->> directory
         vals
         (filter #(<= % 100000))
         (reduce +))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [filesystem (build-tree input)
        directory (directory-sizes (get filesystem "/") ["/"])
        free-space (- 70000000 (get directory ["/"]))
        needed-space (- 30000000 free-space)]
    (->> directory
         vals
         sort
         (remove #(< % needed-space))
         first)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(def sample-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(deftest sample
  (let [input (generator sample-input)]
    (t/is (= 95437 (solve-part-1 input)))))

(deftest sample2
  (let [input (generator sample-input)]
    (t/is (= 24933642 (solve-part-2 input)))))
