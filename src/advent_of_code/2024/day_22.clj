(ns advent-of-code.2024.day-22
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]))

(def data (->> (read/read-file "resources/2024/day_22.txt")
               (utils/parse-int-col)))

(defn mix [n s]
  (bit-xor s n))

(defn prune [s]
  (mod s 16777216))

(defn step-1 [s]
  (let [m (* 64 s)]
    (prune (mix s m))))

(defn step-2 [s]
  (let [d (quot s 32)]
    (prune (mix s d))))

(defn step-3 [s]
  (let [m (* 2048 s)]
    (prune (mix s m))))


(defn process [s]
  (-> (step-1 s)
      (step-2)
      (step-3)))

(def memoized-process (memoize process))

(defn part-1 []
  (->> (map #(last (take 2001 (iterate memoized-process %))) data)
       (reduce +)))

(defn process-2 [s]
  (map #(mod % 10) (take 2001 (iterate memoized-process s))))

(def memoized-process-2 (memoize process-2))

(defn get-changes [s]
  (let [values (memoized-process-2 s)]
    (->> (partition 2 1 values)
         (map-indexed (fn [idx [a b]] [(inc idx) (- b a)]))
         (partition 4 1)
         (map (fn [col] [(map last col) (nth values (first (last col)))])))))

(defn get-value-for-change [change monkey-valuations]
  (get monkey-valuations change))

(defn trim-change-values [change-value]
  (loop [c change-value
         visited #{}
         acc []]
    (if (empty? c)
      (into {} acc)
      (if (contains? visited (ffirst c))
        (recur (rest c) visited acc)
        (recur (rest c) (conj visited (ffirst c)) (conj acc (first c)))))))

(def change-values (->> (map get-changes data)
                        (map trim-change-values)))

(def unique-changes (->> (mapcat #(map first %) change-values)
                         set))

(defn part-2 []
  (->> (map-indexed (fn [idx cv]
                      (vector cv (->> (map (fn [monkey-valuation] (get-value-for-change cv monkey-valuation)) change-values)
                                      (remove nil?)
                                      (reduce +))))
            unique-changes)
       (sort-by last)
       (last)))
