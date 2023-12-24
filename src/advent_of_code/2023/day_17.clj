(ns advent-of-code.2023.day-17
  (:require [advent-of-code.shared.point :as point]
            [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.set :as set]))


(def data (read/read-file "resources/2023/day_17.txt"))

(def finish-point [(dec (count (first data))) (dec (count data))])

(defn get-weight [point]
  (when (get-in data point)
    (Integer/parseInt (str (get-in data (reverse point))))))

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

(def cost-graph (let [width (count (first data))
                      height (count data)
                      points (combo/cartesian-product (range 0 width) (range 0 height))]
                  (reduce (fn [acc point]
                            (assoc acc point (generate-graph-nodes point))) {} points)))

(defn get-direction-amount [[x1 y1] [x2 y2] prev-directions]
  (cond
    (and (= x1 x2) (< y1 y2))
    (merge-with + prev-directions {:up 0 :down 1 :right 0 :left 0})

    (and (= x1 x2) (> y1 y2))
    (merge-with + prev-directions {:up 1 :down 0 :right 0 :left 0})

    (and (< x1 x2) (= y1 y2))
    (merge-with + prev-directions {:up 0 :down 0 :right 1 :left 0})

    (and (> x1 x2) (= y1 y2))
    (merge-with + prev-directions {:up 0 :down 0 :right 0 :left 1})))

(defn get-point-direction [[x1 y1] [x2 y2]]
  (cond
    (and (= x1 x2) (< y1 y2)) :down
    (and (= x1 x2) (> y1 y2)) :up
    (and (< x1 x2) (= y1 y2)) :right
    (and (> x1 x2) (= y1 y2)) :left))

(def memoized-get-point-direction (memoize get-point-direction))

(def initial-point-directions {:up 0, :down 0, :right 0, :left 0})
(defn get-next-paths [path]
  (if (= finish-point (:point path))
    [path]
    (let [next-points (get cost-graph (:point path))
          directions (:directions path)
          visited (:visited path)]
      (if (empty? (set/difference (set (keys next-points)) visited))
        []
        (->> (set/difference (set (keys next-points)) visited)
             (map (fn [p]
                    (let [point-direction (memoized-get-point-direction (:point path) p)]
                      (if (= 3 (get directions point-direction))
                        nil
                        {:point      p
                         :cost       (+ (:cost path) (get next-points p))
                         :visited    (conj visited p)
                         :directions (assoc initial-point-directions point-direction (inc (get directions point-direction)))}))))
             (remove nil?))))))

(defn part-1 []
  (loop [paths [{:point [0 0] :cost 0 :directions {:up 0 :down 0 :right 0 :left 0} :visited #{[0 0]}}]
         iterations 0]
    (println iterations)
    (if (= iterations 30)
      paths
      (if (= #{finish-point} (set (map :point paths)))
        (->> (map :cost paths)
             (apply max))
        (recur (distinct (mapcat get-next-paths paths))
               (inc iterations))))))