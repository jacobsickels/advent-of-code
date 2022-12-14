(ns advent-of-code.2021.day-11
  (:require [advent-of-code-2021.core :as core]
            [clojure.set :as set]))

(def data [[7 7 7 7 8 3 8 3 5 3]
           [2 2 1 7 2 7 2 4 7 8]
           [3 3 5 5 3 1 8 6 4 5]
           [2 2 4 2 6 1 8 1 1 3]
           [7 1 8 2 4 6 8 6 6 6]
           [5 4 4 1 6 4 1 1 1 1]
           [4 7 7 3 8 6 2 3 6 4]
           [5 7 1 7 1 2 5 5 2 1]
           [7 5 4 2 1 2 7 7 2 1]
           [4 5 7 6 6 7 8 3 4 1]])

(def test-data [[1 1 1 1 1]
                [1 9 9 9 1]
                [1 9 1 9 1]
                [1 9 9 9 1]
                [1 1 1 1 1]])

(def test-data-big [[5 4 8 3 1 4 3 2 2 3]
                    [2 7 4 5 8 5 4 7 1 1]
                    [5 2 6 4 5 5 6 1 7 3]
                    [6 1 4 1 3 3 6 1 4 6]
                    [6 3 5 7 3 8 5 4 7 8]
                    [4 1 6 7 5 2 4 6 4 5]
                    [2 1 7 6 8 4 1 7 2 1]
                    [6 8 8 2 8 8 1 1 3 4]
                    [4 8 4 6 8 4 8 5 5 4]
                    [5 2 8 3 7 5 1 5 2 6]])

(defn update-2d [[x y] input func]
  (let [value (nth (nth input y) x)]
    (assoc (vec input) y (assoc (vec (nth input y)) x (func value)))))

(defn flash-point [[x y] input]
  (let [points (remove
                 (fn [[x y]] (or (neg? x) 
                                 (neg? y)
                                 (>= x (count (first input)))
                                 (>= y (count input))))
                 (core/points-around [x y]))]
    (reduce (fn [acc [x y]] (update-2d [x y] acc inc)) 
            input
            points)))

(defn flash-everything [input flashed-points]
  (let [width (count (first input))
        height (count input)
        allowed-points (remove
                         #(contains? flashed-points %)
                         (apply concat (map (fn [h]
                                              (map (fn [w] (list w h))
                                                   (range 0 width)))
                                            (range 0 height))))]
    (reduce
      (fn [acc [x y]] 
        (let [value (nth (nth (first acc) y) x)]
          (if (>= value 10)
            [(flash-point [x y] (first acc)) (conj (second acc) [x y])]
            acc)))
      [input flashed-points] 
      allowed-points))) 

(defn flash-to-finish [input]
  "Flashes can produce other flashes, need to make sure that everything settles"
  (loop [acc (map #(map inc %) input)
         flashed #{}
         last-flashed nil]
    (if (= flashed last-flashed)
      [acc flashed]
      (let [[new-env flashed-points] (flash-everything acc flashed)]
        (recur new-env (set/union flashed-points flashed) flashed)))))

(defn replace-flashes [input width]
  (partition width (map #(if (>= % 10) 0 %) (flatten input))))

(defn part-1 [input]
  (loop [environment input
         iterations 0
         times-flashed 0]
    (if (= iterations 100)
      [environment times-flashed]
      (let [[new-env flashed-points] (flash-to-finish environment)
            flashed-amount (count flashed-points)]
        (recur (replace-flashes new-env (count (first new-env))) 
               (inc iterations) 
               (+ times-flashed flashed-amount))))))

(defn part-2 [input]
  (loop [environment input
         iterations 0
         times-flashed 0]
    (if (= (count (frequencies environment)) 1)
      [environment iterations]
      (let [[new-env flashed-points] (flash-to-finish environment)
            flashed-amount (count flashed-points)]
        (recur (replace-flashes new-env (count (first new-env)))
               (inc iterations)
               (+ times-flashed flashed-amount))))))