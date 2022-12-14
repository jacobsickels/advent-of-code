(ns advent-of-code.2019.day-03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code.shared.read-file :as read]))

(defn get-manhattan-path [[x1 y1] [x2 y2]]
  (sort
    (set
      (concat
        [[x2 y2]]
        (map (fn [x] [x y1]) (apply range (sort [x1 x2])))
        (map (fn [y] [x2 y]) (apply range (sort [y1 y2])))))))

(defn get-point-from-direction
  "[x y] is the starting point for the direction"
  [[x y] dir1]
  (let [direction (list (str (first dir1)) (Integer/parseInt (apply str (rest dir1))))]
    (cond
      (= (first direction) "R") [(+ x (second direction)) y]
      (= (first direction) "D") [x (- y (second direction))]
      (= (first direction) "L") [(- x (second direction)) y]
      (= (first direction) "U") [x (+ y (second direction))])))

(defn get-points-on-line
  [line]
  (loop [segments line
         acc #{[0 0]}
         point [0 0]]
    (if (empty? segments)
      acc
      (let [next (get-point-from-direction point (first segments))]
        (recur (rest segments)
               (set/union acc (get-manhattan-path point next))
               next)))))

(defn manhattan
  []
  (apply min
         (filter #(not (= % 0))
                 (flatten
                   (let [contents (read/read-file "resources/2019/day-03.txt")
                         first (set (get-points-on-line (str/split (first contents) #",")))
                         second (set (get-points-on-line (str/split (second contents) #",")))]
                     (map (fn [[x y]] (list (+ (Math/abs x) (Math/abs y)))) (set/intersection first second)))))))

(defn manhattan-intersections
  []
  (let [contents (read/read-file "resources/2019/day-03.txt")
        first (set (get-points-on-line (str/split (first contents) #",")))
        second (set (get-points-on-line (str/split (second contents) #",")))]
    (set/intersection first second)))


(defn step-through-line
  [[incX incY] line]
  (loop [segments line
         [x y] [0 0]
         count 0
         visited 0]
    (if (and (= incX x) (= incY y))
      visited
      (if (empty? segments)
        (list visited [x y])
        (let [direction (str (first (first segments)))
              value (Integer/parseInt (apply str (rest (first segments))))]
          (cond
            (= direction "R")
            (if (= count value)
              (recur (rest segments) [x y] 0 visited)
              (recur segments [(+ x 1) y] (inc count) (inc visited)))

            (= direction "D")
            (if (= count value)
              (recur (rest segments) [x y] 0 visited)
              (recur segments [x (- y 1)] (inc count) (inc visited)))

            (= direction "L")
            (if (= count value)
              (recur (rest segments) [x y] 0 visited)
              (recur segments [(- x 1) y] (inc count) (inc visited)))

            (= direction "U")
            (if (= count value)
              (recur (rest segments) [x y] 0 visited)
              (recur segments [x (+ y 1)] (inc count) (inc visited)))))))))


(defn steps-to-intersections
  []
  (let [contents (get-file-contents)
        first (str/split (first contents) #",")
        second (str/split (second contents) #",")
        intersections (rest (manhattan-intersections))]
    (sort (map
            (fn [[x y]] (+ x y))
            (zipmap
              (map #(step-through-line % first) intersections
                   (map #(step-through-line % second) intersections)))))))