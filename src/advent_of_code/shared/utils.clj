(ns advent-of-code.shared.utils)

(defn inclusive-range [start end]
  (range start (inc end)))


(defn parse-int-col [col]
  (map #(Integer/parseInt %) col))