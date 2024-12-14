(ns advent-of-code.2024.day-14
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [medley.core :as medley]))

(defn parse-robot [line]
  (let [[x y vx vy] (->> (re-seq #"-?\d+" line)
                         (map #(Integer/parseInt %)))]
    {:position [x y] :velocity [vx vy]}))

(def data (->> (read/read-file "resources/2024/day_14.txt")
               (map parse-robot)))

(defn next-robot-position [robot width height]
  (let [[x y] (:position robot)
        [vx vy] (:velocity robot)]
    (assoc robot :position [(mod (+ x vx) width) (mod (+ y vy) height)])))

(defn move-robots [to-cnt width height]
  (loop [robots data
         cnt 0]
    (if (= cnt to-cnt)
      robots
      (recur (map #(next-robot-position % width height) robots) (inc cnt)))))

(defn filter-robots-in-quadrant [robots [[x1 y1] [x2 y2]]]
  (filter #(let [[rx ry] (:position %)]
             (and (<= x1 rx x2) (<= y1 ry y2))) robots))

(defn part-1 []
  (let [width 101
        height 103
        moved-robots (move-robots 100 width height)
        quad-0 [[0 0] [(dec (quot width 2)) (dec (quot height 2))]]
        quad-1 [[(inc (quot width 2)) 0] [(dec width) (dec (quot height 2))]]
        quad-2 [[0 (inc (quot height 2))] [(dec (quot width 2)) (dec height)]]
        quad-3 [[(inc (quot width 2)) (inc (quot height 2))] [(dec width) (dec height)]]]
    (->> (map #(filter-robots-in-quadrant moved-robots %) [quad-0 quad-1 quad-2 quad-3])
         (map count)
         (reduce *))))

(defn count-robots-in-largest-line [robots]
  (let [set-robots (set (map :position robots))]
    (->> set-robots
         (sort-by last)
         (partition-by last)
         (map #(sort-by first %))
         (map #(medley/partition-between (fn [[x1 y1] [x2 y2]] (> x2 (inc x1))) %))
         (map #(map count %))
         (map sort)
         (map last)
         sort
         last)))

(defn print-robots [robots w h]
  (let [set-robots (set (map :position robots))
        width (range 0 w)
        height (range 0 h)]
    (->> (combo/cartesian-product width height)
         (map #(conj % (if (contains? set-robots %) "#" " ")))
         (sort-by last)
         (partition-by last)
         (map #(sort-by second %))
         (map #(map first %))
         (map #(apply str %)))))

(defn move-robots-until-count [to-cnt width height]
  (loop [robots data
         cnt 0]
    (let [in-line (count-robots-in-largest-line robots)]
      (if (< to-cnt in-line)
        cnt ;;(print-robots robots width height)
        (recur (map #(next-robot-position % width height) robots) (inc cnt))))))

(defn part-2 []
  (let [width 101
        height 103]
    (move-robots-until-count 20 width height)))
;; 20 was an arbitrary number here, I was hoping that the robots would
;; be in a long line to find in the print-robots function
