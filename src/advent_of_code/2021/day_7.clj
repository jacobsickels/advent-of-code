(ns advent-of-code.2021.day-7
  (:require [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "day_07.edn"))

(def test-data [16,1,2,0,4,2,7,1,2,14])

(defn fuel-to-check [input check]
  (reduce + (map #(Math/abs (- % check)) input)))

(defn part-1 [input]
  (apply min (map #(fuel-to-check input %) input)))

(defn fuel-to-check-2 [input check]
  (let [cost (fn [f] (reduce + (core/inclusive-range 0 (Math/abs (- f check)))))]
    (reduce + (map cost input))))

(defn part-2 [input]
  (second (apply min-key second (map-indexed #(list %1 (fuel-to-check-2 input %2)) 
                                             (core/inclusive-range 0 (apply max input))))))
