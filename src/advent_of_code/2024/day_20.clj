(ns advent-of-code.2024.day_20
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_20.txt")
               (partition-by empty?)))

(defn find-symbols [s]
  (->> (map-indexed (fn [i line]
                      (map-indexed
                        (fn [j space] (list [j i] space))
                        (str/split line #""))) (first data))
       (mapcat (fn [col] (filter #(= s (second %)) col)))
       (map first)))

(def walls (set (find-symbols "#")))
(def start (first (find-symbols "S")))
(def end (first (find-symbols "E")))
(def spaces (find-symbols "."))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))


(defn get-next-point [point previous-point]
  (let [points-around (points/cardinal-points-around point)]
    (first (set/difference (set points-around) (set/union walls (set [previous-point]))))))

(defn walk []
  (loop [point start
         previous-point nil
         acc {start {:cost 0}}]
    (if (= point end)
      acc
      (let [next-point (get-next-point point previous-point)]
        (recur next-point point (assoc (reduce (fn [acc [k v]] (assoc acc k {:cost (inc (:cost v))})) {} acc) next-point {:cost 0}))))))

(defn cheat-points-around [[x y]]
  (list [(+ x 2) y]
        [(- x 2) y]
        [x (+ y 2)]
        [x (- y 2)]))

(defn get-cheat-points [point cost-map]
  (let [cheat-points (cheat-points-around point)]
    (assoc cost-map
      point
      (merge
        (get cost-map point)
        {:cheats (reduce (fn [acc p1]
                           (let [point-cost (:cost (get cost-map point))
                                 p1-cost (:cost (get cost-map p1))]
                             (println point p1 (manhattan-distance point p1))
                             (if (nil? p1-cost)
                               acc
                               (let [saves (- point-cost p1-cost 2)]
                                 (if (<= saves 0)
                                   acc
                                   (assoc acc p1 {:cost p1-cost :saves (- point-cost p1-cost (manhattan-distance point p1))}))))))
                         {}
                         cheat-points)}))))

(defn get-all-cheat-points []
  (let [walked (walk)]
    (reduce (fn [acc point] (get-cheat-points point acc)) walked (keys walked))))

(defn get-cheat-counts []
  (let [cost-map (get-all-cheat-points)]
    (->> (reduce (fn [acc [k v]] (assoc acc k (map (fn [[k1 v1]] (:saves v1)) (:cheats v)))) {} cost-map)
         (vals)
         (remove empty?)
         (apply concat)
         frequencies)))

(defn part-1 []
  (let [cheat-counts (get-cheat-counts)]
    (->> (map (fn [[k v]] (if (>= k 100) v 0)) cheat-counts)
         (reduce +))))
;; ======================================

(defn is-invalid-point? [[x y]]
  (or (neg? x) (neg? y)))

(defn get-manhattan-20-points-around [[x y]]
  (let [diff-range (range -20 (inc 20))]
    (->> (map (fn [[dx dy]] [(+ dx x) (+ dy y)]) (combo/cartesian-product diff-range diff-range))
         (remove (fn [[x1 y1]] (> (manhattan-distance [x y] [x1 y1]) 20))))))

(defn get-cheat-points-20 [point cost-map]
  (let [cheat-points (get-manhattan-20-points-around point)]
    (assoc cost-map
      point
      (merge
        (get cost-map point)
        {:cheats (reduce (fn [acc p1]
                           (let [point-cost (:cost (get cost-map point))
                                 p1-cost (:cost (get cost-map p1))]
                             (if (nil? p1-cost)
                               acc
                               (let [saves (- point-cost p1-cost (manhattan-distance point p1))]
                                 (if (<= saves 0)
                                   acc
                                   (assoc acc p1 {:cost p1-cost :saves (- point-cost p1-cost (manhattan-distance point p1))}))))))
                         {}
                         cheat-points)}))))

(defn get-all-cheat-points-20 []
  (let [walked (walk)]
    (reduce (fn [acc point] (get-cheat-points-20 point acc)) walked (keys walked))))

(defn get-cheat-counts-20 []
  (let [cost-map (get-all-cheat-points-20)]
    (->> (reduce (fn [acc [k v]] (assoc acc k (map (fn [[k1 v1]] (:saves v1)) (:cheats v)))) {} cost-map)
         (vals)
         (remove empty?)
         (apply concat)
         frequencies)))

(defn part-2 []
  (let [cheat-counts (get-cheat-counts-20)]
    (->> (reduce (fn [acc [k v]] (if (>= k 100) (assoc acc k v) acc)) {} cheat-counts)
         vals
         (reduce +))))