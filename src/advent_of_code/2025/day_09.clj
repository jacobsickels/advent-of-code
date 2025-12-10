(ns advent-of-code.2025.day-09
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import (org.locationtech.jts.geom Coordinate GeometryFactory)))

(def test-data (->> (read/read-file "resources/2025/day_09.txt")))

(def test-points (->> (map #(str/split % #"") test-data)
                      (map-indexed (fn [y line]
                                     (map-indexed (fn [x space] [space x y]) line)))
                      (apply concat)))

(def test-corners [[7 1]
                   [11 1]
                   [11 7]
                   [9 7]
                   [9 5]
                   [2 5]
                   [2 3]
                   [7 3]])

(def corners (->> (read/read-file "resources/2025/day_09_points.txt")
                  (map #(map (fn [n] (Integer/parseInt n)) (str/split % #",")))))

(defn find-area [[p1x p1y] [p2x p2y]]
  (let [[x1 x2] (sort [p1x p2x])
        [y1 y2] (sort [p1y p2y])]
    (reduce * [(inc (- x2 x1)) (inc (- y2 y1))])))

(defn find-areas [points point]
  (let [filtered (remove #(= % point) points)]
    (apply merge (map #(hash-map (set [% point]) (find-area point %)) filtered))))

(defn find-all-areas [points]
  (apply merge (map #(find-areas points %) points)))

(defn part-1 []
  (->> (find-all-areas corners)
       (sort-by second)
       (last)
       (last)))

(defn create-polygon [coordinates]
  (let [gf (GeometryFactory.)
        coords-array (into-array Coordinate (map (fn [[x y]] (Coordinate. x y)) coordinates))]
    (.createPolygon gf coords-array)))

(defn polygon-contains-polygon? [poly1 poly2]
  (.contains poly1 poly2))

(defn create-polygon-from-corners [corner-points]
  (let [[x1 y1] (first corner-points)
        [x2 y2] (second corner-points)]
    (create-polygon [[x1 y1] [x1 y2] [x2 y2] [x2 y1] [x1 y1]])))

(def boundary (create-polygon (conj corners (last corners))))

(defn part-2 []
  (let [all-polygons (->> (find-all-areas corners)
                          (map first))]
    (->> (filter #(polygon-contains-polygon? boundary (create-polygon-from-corners %)) all-polygons)
         (map (fn [points] (find-area (first points) (second points))))
         (sort)
         (last))))