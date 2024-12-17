(ns advent-of-code.2024.day-16
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.point :as point]
            [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_16.txt")
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

(defn get-direction [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (cond
      (and (= x1 x2) (< y1 y2)) :down
      (and (= x1 x2) (> y1 y2)) :up
      (and (= y1 y2) (< x1 x2)) :right
      (and (= y1 y2) (> x1 x2)) :left
      :else nil)))

(def filtered-directions {:up :down
                          :down :up
                          :right :left
                          :left :right})


(defn get-points-around-direction [[x y direction]]
  (let [points-around (set (points/cardinal-points-around [x y]))
        directions-around (map #(conj % (get-direction [x y] %)) (set/difference points-around walls))]
    (remove #(= direction(get filtered-directions (last %))) directions-around)))

(defn get-next-points-cost [p costs]
  (let [points-around (get-points-around-direction p)
        [cost direction] (get costs (butlast p))]
    (reduce (fn [acc [x y d]]
              (if (= direction d)
                (merge acc {[x y] [(+ cost 1) d]})
                (merge acc {[x y] [(+ cost 1001) d]})))
            {}
         points-around)))

(defn safe-merge-cost-maps [map1 map2]
  (merge-with (fn [a b]
                (if (< (first a) (first b))
                  a
                  b))
              map1
              map2))

(defn get-visited-costs [cost-map]
  (set (map (fn [[k v]] (conj k (last v))) cost-map)))

(defn part-1 []
  (loop [cost {start [0 :right]}
         visited #{}
         ittr 0]
    (if (= ittr 500) ;; This is just a guess at iterations so it doesn't blow up, not sure how to look for end condition
      (get cost end)
      (let [current-cost-visited (get-visited-costs cost)
            points-to-check (set/difference current-cost-visited visited)
            point-costs (map #(get-next-points-cost % cost) points-to-check)]
        (recur (reduce (fn [acc m] (safe-merge-cost-maps acc m)) cost point-costs)
               current-cost-visited
               (inc ittr))))))