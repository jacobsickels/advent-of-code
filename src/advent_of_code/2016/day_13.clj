(ns advent-of-code.2016.day-13
  (:require [advent-of-code.shared.point :as points]
            [clojure.set :as set]))

(def input 1362)

(defn is-wall? [[x y]]
  (let [int-num (+ (* x x)
                   (* 3 x)
                   (* 2 y x)
                   y
                   (* y y)
                   input)]
    (->> (Integer/toBinaryString int-num)
         (filter #(= % \1))
         count
         odd?)))

(defn walkable-points [[x y]]
  (let [points-around (->> (points/cardinal-points-around [x y])
                           (filter (fn [[x y]] (and (<= 0 x) (<= 0 y) (not (is-wall? [x y]))))))]
    points-around))

(defn walk [destination]
  (loop [points [[1 1]]
         visited #{[1 1]}
         iterations 0]
    (if (contains? (set points) destination)
      iterations
      (let [next-points (set (apply concat (map walkable-points points)))]
        (recur (set/difference next-points visited)
               (set/union visited next-points)
               (inc iterations))))))

(defn part-1 [destination] ;; 82
  (walk destination))

(defn part-2 [] ;; 138
  (loop [points [[1 1]]
         visited #{[1 1]}
         iterations 0]
    (if (= iterations 50)
      (count visited)
      (let [next-points (set (apply concat (map walkable-points points)))]
        (recur (set/difference next-points visited)
               (set/union visited next-points)
               (inc iterations))))))