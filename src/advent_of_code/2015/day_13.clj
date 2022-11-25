(ns advent-of-code.2015.day-13
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [sign (if (str/includes? line "gain") 1 -1)
        amount (* sign (Integer/parseInt (re-find #"\d+" line)))
        col (str/split line #" ")]
    {[(first col) (apply str (drop-last (last col)))] amount}))

(def test-data (->> (read/read-file "resources/2015/day_13_test.txt")
                    (map parse-line)
                    (apply merge)))

(def data (->> (read/read-file "resources/2015/day_13.txt")
               (map parse-line)
               (apply merge)))

(defn get-unique-names [col]
  (set (apply concat (keys col))))

(defn make-loop [arrangement]
  (conj arrangement (last arrangement)))

(defn process-loop [loop col]
  "Get(s) on the collection return zero for part 2, I didn't add myself to the maps"
  (reduce #(+ %1 (or (get col %2) 0) (or (get col (reverse %2)) 0))
          0
          (partition 2 1 loop)))

(defn find-optimal-arrangement [unique-names col]
  (loop [arr (combo/permutations unique-names)
         acc {}]
    (if (empty? arr)
      (apply max-key val acc)
      (recur (rest arr)
             (assoc acc (first arr) (process-loop (make-loop (first arr)) col))))))


(defn part-1 [col]
  (find-optimal-arrangement (get-unique-names col) col))

(defn part-2 [col]
  (find-optimal-arrangement (set/union (get-unique-names col) #{"Jacob"}) col))
