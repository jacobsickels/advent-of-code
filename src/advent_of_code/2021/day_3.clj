(ns advent-of-code.2021.day-3
  (:require [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

(def data (read/read-file "resources/day_3.txt"))

(def test-data ["00100"
                "11110"
                "10110"
                "10111"
                "10101"
                "01111"
                "00111"
                "11100"
                "10000"
                "11001"
                "00010"
                "01010"])

(defn part-1 []
  (let [f        (map frequencies
                      (map #(map (fn [d] (get d %)) data)
                           (range 0 (count (first data)))))
        maximums (Integer/parseInt (apply str (map first (map #(apply max-key val %) f))) 2)
        minumums (Integer/parseInt (apply str (map first (map #(apply min-key val %) f))) 2)]
    (* maximums minumums)))

;; Part 2

(defn get-rating [input min-max]
  (loop [acc input
         place 0]
    (if (<= (count acc) 1)
      (Integer/parseInt (first acc) 2)
      (let [[front back] (partition-by #(get % place) acc)
            next (cond 
                   (= (count front) (count back))
                   (if (= min-max :max) back front)
        
                   (> (count front) (count back))
                   (if (= min-max :max) front back)
        
                   (< (count front) (count back))
                   (if (= min-max :max) back front))]
        (recur next (inc place))))))

(defn part-2 [input]
  (let [oxygen-rating (get-rating (sort input) :max)
        co2-rating    (get-rating (sort input) :min)]
    (* oxygen-rating co2-rating)))
