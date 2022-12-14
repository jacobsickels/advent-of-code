(ns advent-of-code.2021.day-13
  (:require [advent-of-code-2021.core :as core]
            [clojure.set :as set]
            [advent-of-code.shared.read-file :as read]))

(def data (partition 2 (read/load-edn "day_13.edn")))
(def data-folds [[:x 655]
                 [:y 447]
                 [:x 327]
                 [:y 223]
                 [:x 163]
                 [:y 111]
                 [:x 81]
                 [:y 55]
                 [:x 40]
                 [:y 27]
                 [:y 13]
                 [:y 6]])

(def test-data [[6, 10]
                [0, 14]
                [9, 10]
                [0, 3]
                [10, 4]
                [4, 11]
                [6, 0]
                [6, 12]
                [4, 1]
                [0, 13]
                [10, 12]
                [3, 4]
                [3, 0]
                [8, 4]
                [1, 10]
                [2, 14]
                [8, 10]
                [9, 0]])

(defn get-fold-areas [input fold-type fold]
  (let [sorted-x (sort-by first input)
        sorted-y (sort-by second input)]
    (cond
      (= fold-type :x)
      [(first (partition-by (fn [[x y]] (< x fold)) sorted-x))
       (last (partition-by (fn [[x y]] (> x fold)) sorted-x))]

      (= fold-type :y)
      [(first (partition-by (fn [[x y]] (< y fold)) sorted-y))
       (last (partition-by (fn [[x y]] (> y fold)) sorted-y))])))

(defn perform-y-fold [input fold]
  (let [[top bottom] (get-fold-areas input :y fold)]
    (set (concat top (map (fn [[x y]] [x (- fold (- y fold))]) bottom)))))

(defn perform-x-fold [input fold]
  (let [[left right] (get-fold-areas input :x fold)]
    (set (concat left (map (fn [[x y]] [(- fold (- x fold)) y]) right)))))

(defn part-1 []
  (perform-x-fold data (second (first data-folds))))


(defn print-output [input]
  (let [w-range        (range 0 (inc (apply max (map first input))))
        h-range        (range 0 (inc (apply max (map second input))))
        allowed-points (apply concat (map (fn [h]
                                            (map (fn [w] (list w h))
                                                 w-range))
                                          h-range))]
    (println (apply max (map first input)))
    (re-seq #".{1,39}"
               (apply str
                    (map (fn [point] (if (contains? (set input) point) "#" " "))
                         allowed-points)))))


(defn part-2 []
  (print-output (reduce (fn [acc [fold-type fold]]
                          (if (= fold-type :x)
                            (perform-x-fold acc fold)
                            (perform-y-fold acc fold)))
                        data
                        data-folds)))