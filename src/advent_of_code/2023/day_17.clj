(ns advent-of-code.2023.day-17
  (:require [advent-of-code.shared.point :as point]
            [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.data.priority-map :refer [priority-map]]))

;; djikstra with costs for every step between 1 -3 in each direction
;; Starting in top left corner it could be

; [0 0] -> [1 0 4] [2 0 5] [3 0 8] [1 0 3] [2 0 6] [3 0 9]

;; there might be a way to modify this, but you can't do directional points because it can't go more than 3 in one direction

(def data (read/read-file "resources/2023/day_17.txt"))

(defn get-weight [point]
  (when (get-in data point)
    (Integer/parseInt (str(get-in data (reverse point))))))

(defn get-weight-points-between [p1 p2]
  (->> (map get-weight (points/points-between p1 p2))
       (drop 1)
       (reduce +)))

(defn generate-graph-nodes [[x y]]
  (reduce (fn [acc point]
            (if (nil? (get-in data point))
              acc
              (assoc acc point (Integer/parseInt (str (get-in data point))))))
          {}
          (point/cardinal-points-around [x y])))

(defn generate-dijkstra-graph []
  (let [width  (count (first data))
        height (count data)
        points (combo/cartesian-product (range 0 width) (range 0 height))]
    (reduce (fn [acc point]
              (assoc acc point (generate-graph-nodes point))) {} points)))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start target f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (if (= target (take 2 v))
        d
        (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
          (recur (merge-with min (pop q) dist) (assoc r v d))))
      r)))

(defn get-direction [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2)
    (if (< y1 y2) :down :up)

    (= y1 y2)
    (if (< x1 x2) :right :left)))

(def opposite-directions {:up :down
                          :down :up
                          :right :left
                          :left :right})


(defn get-next-points [n]
  (let [[x y distance direction] n
        max-distance 3]
    (->> {:down  (sort-by second (points/points-between [x (inc y)] [x (+ y max-distance)]))
          :up    (sort-by second > (points/points-between [x (dec y)] [x (- y max-distance)]))
          :right (sort-by first (points/points-between [(inc x) y] [(+ x max-distance) y]))
          :left  (sort-by first > (points/points-between [(dec x) y] [(- x max-distance) y]))}

         ;; Get valid next points cart can travel to, this removes points in direction cart is going
         (reduce (fn [acc [k v]]
                   (cond
                     ;; We are at the start
                     (nil? direction) (assoc acc k v)

                     ;; We cannot backtrack
                     (= k (get opposite-directions direction))
                     acc

                     :else (cond
                             (= direction k)
                             (assoc acc k (drop-last distance v))

                             :else (assoc acc k v))))
                 {})

         ;; Add distances to new points
         (reduce (fn [acc [k v]]
                   (assoc acc k (map-indexed (fn [i p] (conj p (inc i) k)) v)))
                 {})

         ;; generate graph
         (reduce (fn [acc [k v]] (merge acc (reduce (fn [graph p]
                                                      (if (nil? (get-weight (take 2 p)))
                                                        graph
                                                        (assoc graph p (get-weight-points-between [x y] (take 2 p))))) {} v)))
                 {}))))

(defn part-1 [test?]
  (dijkstra [0 0 0 :right] (if test? [12 12] [140 140]) (fn [n] (get-next-points n))))
