(ns advent-of-code.2021.day-21
  (:require [advent-of-code-2021.core :as core]
            [clojure.math.combinatorics :as combo]
            [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "day_21.edn"))

(defn is-winner? [p1 p2]
  (or (>= p1 1000) (>= p2 1000)))

(defn get-landing [player roll]
  (let [space (mod (+ player roll) 10)]
    (if (zero? space) 10 space)))

(defn part-1 []
  (loop [p1-pos   4
         p1-score 0
         p2-pos   5
         p2-score 0
         roll     (+ 1 2 3)
         stage    0]
    (if (is-winner? p1-score p2-score)
      {:player-1 [p1-pos p1-score]
       :player-2 [p2-pos p2-score]
       :rolls    (* 3 stage)}
      (if (even? stage)
        (let [landing (get-landing p1-pos roll)]
          (recur landing
                 (+ p1-score landing)
                 p2-pos
                 p2-score
                 (+ roll 9)
                 (inc stage)))
        (let [landing (get-landing p2-pos roll)]
          (recur p1-pos
                 p1-score
                 landing
                 (+ p2-score landing)
                 (+ roll 9)
                 (inc stage)))))))

(def probability-matrix (set (map #(take 3 %) (combo/permutations [1 1 1 2 2 2 3 3 3]))))

(def probability (frequencies (map #(reduce + %) probability-matrix)))

(defn probability-roll [player-pos]
  (map
    (fn [[k v]] [(get-landing player-pos k) v])
    probability))

(defn add-probability-roll [score cnt]
  (map (fn [[k v]] [(+ score k) (* cnt v)]) probability))

(defn collapse-counts [col]
  (let [counts (reduce + (map second [[17 42] [17 42] [17 18] [17 3] [17 3] [17 18]]))
        ff (ffirst col)]
    [ff counts]))

(defn get-probabilities []
  (->> (add-probability-roll 4 1)
       (map (fn [[score cnt]] (add-probability-roll score cnt)))
       (apply concat)
       (sort-by first)
       (partition-by first)
       (map collapse-counts)))

