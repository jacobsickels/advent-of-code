(ns advent-of-code.2021.day-15-clean
  (:require [clojure.string :as str]
            [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

(require '[clojure.data.priority-map :refer [priority-map]])



(def test-data (map #(map (fn [i] (Integer/parseInt i))
                          (str/split % #""))
                    (read/read-file "resources/day_15_p.txt")))

(def data (map #(map (fn [i] (Integer/parseInt i))
                     (str/split % #""))
               (read/read-file "resources/day_15.txt")))

(defn get-point-weight [[x y] input]
  (let [width  (count (first input))
        height (count input)]
    (when (not (or (neg? x) (neg? y) (>= x width) (>= y height)))
      (nth (nth input y) x))))

(defn points-around [[x y]]
  (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn get-successor-map [point]
  (into {} (remove (fn [[k v]] (nil? v))
                   (reduce (fn [acc p] (conj acc {p (get-point-weight p data)}))
                           {}
                           (points-around point)))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.
 
  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) 
                     (remove-keys r) 
                     (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn part-1 [] (get (dijkstra [0 0] get-successor-map) [99 99]))

(defn get-point-weight-2 [[x y] input]
  (let [width  (count (first input))
        height (count input)]
    (when (not (or (neg? x)
                   (neg? y)
                   (>= x (* width 5))
                   (>= y (* height 5))))
      (nth (nth input (mod y height)) (mod x width)))))

(defn get-successor-map-2 [point]
  (let [width       (count (first data))
        height (count data)]
    (into {} (remove (fn [[k v]] (nil? v))
                     (reduce (fn [acc p] (conj acc {p (let [found (get-point-weight-2 p data)
                                                            incced (when found (+ found
                                                                                  (quot (first p) width)
                                                                                  (quot (second p) height)))]
                                                        (if (and incced (> incced 9))
                                                          (- incced 9) incced))}))
                             {}
                             (points-around point))))))


(defn part-2 [] (get (dijkstra [0 0] get-successor-map-2) [499 499]))