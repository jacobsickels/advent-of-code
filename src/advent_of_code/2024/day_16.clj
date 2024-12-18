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





;; [{ :point [1 13] :direction :right :count 1 }] { [1 13] 0 }
;; [{ :point [1 12] :direction :up :count 1 } { :point [2 13] :direction :right :cost 1 :count 1 }]

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
(defn get-next-paths [path]
  (let [[x y] (:point path)
        direction (:direction path)
        count (:count path)
        points-around (set (points/cardinal-points-around [x y]))
        directions-around (map #(hash-map :point %  :direction (get-direction [x y] %) :count count) (set/difference points-around walls))]
    (remove #(= direction (get filtered-directions (:direction %))) directions-around)))

(defn get-next-path-costs [paths costs]
  (loop [p paths
         acc []]
    (if (empty? p)
      (flatten acc)
      (let [next-paths (get-next-paths (first p))]
        (recur (rest p)
               (conj acc (map (fn [next-path] (if (= (:direction (first p)) (:direction next-path))
                                                (assoc next-path :cost (+ (get costs (:point (first p))) 1))
                                                (assoc next-path :cost (+ (get costs (:point (first p))) 1001))))
                              next-paths)))))))

(defn paths-to-cost [paths cost]
  (reduce (fn [acc path]
            (let [point (:point path)
                  path-cost (:cost path)]
              (cond
                (and (contains? cost point) (= (get cost point) path-cost))
                [(conj (first acc) path) (second acc)]

                (and (contains? cost point) (< (get cost point) path-cost))
                [(first acc) (second acc)]

                :else
                [(conj (first acc) path) (assoc (second acc) point path-cost)])))

          [[] cost]
          paths))
(defn part-1 []
  (loop [paths [{ :point start :direction :right :count 1}]
         costs {start 0}]
    (if (empty? paths)
      (get costs end)
      (let [next-paths (get-next-path-costs paths costs)
            [next-paths next-costs] (paths-to-cost next-paths costs)]
        (recur next-paths next-costs)))))







































(defn get-points-around-direction [[x y direction]]
  (let [points-around (set (points/cardinal-points-around [x y]))
        directions-around (map #(conj % (get-direction [x y] %)) (set/difference points-around walls))]
    (remove #(= direction (get filtered-directions (last %))) directions-around)))

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
                (if (< (first a) (first b)) a b))
              map1
              map2))

(defn get-visited-costs [cost-map]
  (set (map (fn [[k v]] (conj k (last v))) cost-map)))

;(defn part-1 []
;  (loop [cost {start [0 :right]}
;         visited #{}]
;    (let [current-cost-visited (get-visited-costs cost)]
;      (if (= visited current-cost-visited)
;        (get cost end)
;        (let [points-to-check (set/difference current-cost-visited visited)
;              point-costs (map #(get-next-points-cost % cost) points-to-check)]
;          (recur (reduce (fn [acc m] (safe-merge-cost-maps acc m)) cost point-costs)
;                 current-cost-visited))))))