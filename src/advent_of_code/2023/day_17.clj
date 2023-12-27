(ns advent-of-code.2023.day-17
  (:require [advent-of-code.shared.point :as point]
            [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.data.priority-map :refer [priority-map]]
            [loom.alg-generic :as loom-alg]
            [clojure.set :as set]))


(def data (read/read-file "resources/2023/day_17.txt"))

(def finish-point [(dec (count (first data))) (dec (count data))])

(defn generate-graph-nodes [[x y]]
  (reduce (fn [acc point]
            (if (nil? (get-in data point))
              acc
              (assoc acc point (Integer/parseInt (str (get-in data point))))))
          {}
          (point/cardinal-points-around [x y])))

;(def cost-graph (let [width (count (first data))
;                      height (count data)
;                      points (combo/cartesian-product (range 0 width) (range 0 height))]
;                  (reduce (fn [acc point]
;                            (assoc acc point (generate-graph-nodes point))) {} points)))

(defn get-weight [point]
  (when (get-in data (reverse point))
    (Integer/parseInt (str (get-in data (reverse point))))))

(def initial-point-directions {:up 0 :down 0 :right 0 :left 0})

(defn is-invalid-point? [[x y]]
  (or (neg? x)
      (neg? y)
      (>= x (count (first data)))
      (>= y (count data))))

(defn successors [node]
  (let [[[x y] direction-costs] node
        {right :right left :left up :up down :down} direction-costs]
    (->> (concat
           (when (and (zero? left) (< right 3))
             [(vector [(inc x) y] (merge initial-point-directions {:right (inc right)}))])

           (when (and (zero? right) (< left 3))
             [(vector [(dec x) y] (merge initial-point-directions {:left (inc left)}))])

           (when (and (zero? down) (< up 3))
             [(vector [x (dec y)] (merge initial-point-directions {:up (inc up)}))])

           (when (and (zero? up) (< down 3))
             [(vector [x (inc y)] (merge initial-point-directions {:down (inc down)}))]))
         (remove #(is-invalid-point? (first %))))))

(defn distance [node-1 node-2]
  (let [[node-1-point _] node-1
        [node-2-point _] node-2]
    (get-weight node-2-point)))

(defn generate-possible-endings []
  [[finish-point {:up 0 :down 0 :right 1 :left 0}]
   [finish-point {:up 0 :down 0 :right 2 :left 0}]
   [finish-point {:up 0 :down 0 :right 3 :left 0}]
   [finish-point {:up 0 :down 1 :right 0 :left 0}]
   [finish-point {:up 0 :down 2 :right 0 :left 0}]
   [finish-point {:up 0 :down 3 :right 0 :left 0}]])

(defn part-1 []
  (->> (generate-possible-endings)
       (map #(loom-alg/dijkstra-path-dist successors distance [[0 0] {:up 0 :down 0 :right 0 :left 0}] %))
       (map last)
       (apply min)))

;(->> (map get-weight (points/points-between p1 p2))
;       (remove nil?)
;       (drop 1)
;       (reduce +))

(defn get-weight-points-between [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    (if (< y1 y2)
      (->> (points/points-between [x1 y1] [x2 y2])
           (map get-weight)
           (remove nil?)
           (drop 1)
           (reduce +))
      (->> (points/points-between [x1 y1] [x2 y2])
           (map get-weight)
           (remove nil?)
           reverse
           (drop 1)
           (reduce +)))
    (if (< x1 x2)
      (->> (points/points-between [x1 y1] [x2 y2])
           (map get-weight)
           (remove nil?)
           (drop 1)
           (reduce +))
      (->> (points/points-between [x1 y1] [x2 y2])
           (map get-weight)
           (remove nil?)
           reverse
           (drop 1)
           (reduce +)))))
(defn distance-2 [node-1 node-2]
  (let [[node-1-point _] node-1
        [node-2-point _] node-2
        weight (get-weight-points-between node-1-point node-2-point)]
    weight))

(defn successors-2 [node]
  (let [[[x y] direction-costs] node
        {right :right left :left up :up down :down} direction-costs
        to-return (->> (concat
                         (when (and (zero? left) (<= 4 right 9))
                           [(vector [(inc x) y] (merge initial-point-directions {:right (inc right)}))])

                         (when (and (zero? left) (< right 4))
                           [(vector [(+ x 4) y] (merge initial-point-directions {:right (+ right 4)}))])

                         (when (and (zero? right) (<= 4 left 9))
                           [(vector [(dec x) y] (merge initial-point-directions {:left (inc left)}))])

                         (when (and (zero? right) (< left 4))
                           [(vector [(- x 4) y] (merge initial-point-directions {:left (+ left 4)}))])

                         (when (and (zero? down) (<= 4 up 9))
                           [(vector [x (dec y)] (merge initial-point-directions {:up (inc up)}))])

                         (when (and (zero? down) (< up 4))
                           [(vector [x (- y 4)] (merge initial-point-directions {:up (+ up 4)}))])

                         (when (and (zero? up) (<= 4 down 9))
                           [(vector [x (inc y)] (merge initial-point-directions {:down (inc down)}))])

                         (when (and (zero? up) (< down 4))
                           [(vector [x (+ y 4)] (merge initial-point-directions {:down (+ down 4)}))]))
                       (remove #(is-invalid-point? (first %))))]
    to-return))


(defn generate-possible-endings-2 []
  (let [finish-range (range 1 11)]
    (concat
      (map #(vector finish-point {:up 0 :down 0 :right % :left 0}) finish-range)
      (map #(vector finish-point {:up 0 :down % :right 0 :left 0}) finish-range))))

(defn part-2 []
  (->> (generate-possible-endings-2)
       (map #(loom-alg/dijkstra-path-dist successors-2 distance-2 [[0 0] {:up 0 :down 0 :right 0 :left 0}] %))
       (map last)
       (remove nil?)
       (apply min)))
