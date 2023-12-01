(ns advent-of-code.core)

(defn s [col] (apply + col))
(defn p [col] (apply + (map #(/ 1 %) col)))

(defn testing [])