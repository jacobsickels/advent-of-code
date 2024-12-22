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
(def spaces (find-symbols "."))



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

(defn get-cost [p1 p2]
  (if (not= (last p1) (last p2)) 1001 1))

(defn get-next-points [path]
  (let [point (:point path)
        direction (:direction path)
        points-around (points/cardinal-points-around point)]
    (->> (set/difference (set points-around) walls)
         (map #(conj (vec %) (get-direction point (vec %))))
         (remove #(= (last %) (get filtered-directions direction))))))

(defn update-visited [path visited]
  (let [k (conj (:point path) (:direction path))
        v (:cost path)]
    (cond
      (contains? visited k)
      (let [current-cost (:cost (get visited k))]
        (if (< v current-cost)
          (assoc visited k (assoc (get visited k) :cost v))
          visited))

      :else
      (assoc visited k {:cost v}))))

(defn get-next-paths [path visited]
  (let [point (:point path)
        cost (:cost path)
        direction (:direction path)
        next-points (get-next-points path)]
    (->> (map #(hash-map :point (vec (take 2 %))
                         :points (conj (:points path) (vec (take 2 %)))
                         :cost (+ cost (get-cost (conj point direction) %))
                         :direction (last %))
              next-points)
         (filter #(let [check (conj (:point %) (:direction %))
                        curr-path-cost (:cost %)
                        value (:cost (get visited check))]
                    (cond
                      (not (nil? value))
                      (< curr-path-cost value)

                      :else true))))))



(defn part-1 []
  (loop [paths [{:point start :points #{start} :direction :right :cost 0}]
         visited {(conj start :right) {:cost 0 :count 1}}
         acc []
         ittr 0]
    (if (empty? paths)
      (->> (map :cost acc)
           (sort)
           (first))
      (let [next-paths (mapcat #(get-next-paths % visited) paths)]
        (recur next-paths
               (reduce (fn [acc path] (update-visited path acc))
                       visited
                       next-paths)
               (concat acc (filter #(= end (:point %)) next-paths))
               (inc ittr))))))

(defn part-2 []
  (loop [paths [{:point start :points #{start} :direction :right :cost 0}]
         visited {(conj start :right) {:cost 0 :count 1}}
         acc []
         ittr 0]
    (if (empty? paths)
      (let [best-cost (->> (map :cost acc)
                           (sort)
                           (first))]
        (->> (filter #(= best-cost (:cost %)) acc)
             (mapcat :points)
             set
             count))
      (let [next-paths (mapcat #(get-next-paths % visited) paths)]
        (recur next-paths
               (reduce (fn [acc path] (update-visited path acc))
                       visited
                       next-paths)
               (concat acc (filter #(= end (:point %)) next-paths))
               (inc ittr))))))







































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