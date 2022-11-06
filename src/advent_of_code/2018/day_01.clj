(ns advent-of-code.2018.day-01
  (:require [advent-of-code.shared.read-file :as read]))

;(read/load-edn "2018/day_01.edn") => 556
(defn part-1 [col]
  (reduce + col))

;(read/load-edn "2018/day_01.edn") => 448
(defn part-2 [col]
  (loop [frequency 0
         pointer 0
         found #{}]
    (if (found frequency)
      frequency
      (let [next-frequency (+ frequency (get col pointer))]
        (recur next-frequency
               (mod (inc pointer) (count col))
               (conj found frequency))))))