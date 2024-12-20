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
;(defn get-next-paths [path]
;  (let [[x y] (:point path)
;        direction (:direction path)
;        count (:count path)
;        points-around (set (points/cardinal-points-around [x y]))
;        directions-around (map #(hash-map :point %  :direction (get-direction [x y] %) :count count) (set/difference points-around walls))]
;    (remove #(= direction (get filtered-directions (:direction %))) directions-around)))

;(defn get-next-path-costs [paths costs]
;  (loop [p paths
;         acc []]
;    (if (empty? p)
;      (flatten acc)
;      (let [next-paths (get-next-paths (first p))]
;        (recur (rest p)
;               (conj acc (map (fn [next-path] (if (= (:direction (first p)) (:direction next-path))
;                                                (assoc next-path :cost (+ (get costs (:point (first p))) 1))
;                                                (assoc next-path :cost (+ (get costs (:point (first p))) 1001))))
;                              next-paths)))))))

;(defn paths-to-cost [paths cost]
;  (reduce (fn [acc path]
;            (let [point (:point path)
;                  path-cost (:cost path)]
;              (cond
;                (and (contains? cost point) (= (get cost point) path-cost))
;                [(conj (first acc) path) (second acc)]
;
;                (and (contains? cost point) (< (get cost point) path-cost))
;                [(first acc) (second acc)]
;
;                :else
;                [(conj (first acc) path) (assoc (second acc) point path-cost)])))
;
;          [[] cost]
;          paths))
;(defn part-1 []
;  (loop [paths [{ :point start :direction :right :count 1}]
;         costs {start 0}]
;    (if (empty? paths)
;      (get costs end)
;      (let [next-paths (get-next-path-costs paths costs)
;            [next-paths next-costs] (paths-to-cost next-paths costs)]
;        (recur next-paths next-costs)))))




;; NOT MY CODE
(def ^:private inf Double/POSITIVE_INFINITY)

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisited
  neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get g curr))))

(defn dijkstra
  "Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a map of nodes to map of neighboring nodes and associated cost.
  Optionally, specify destination node to return once cost is known"
  ([g src]
   (dijkstra g src nil))
  ([g src dst]
   (loop [costs     (assoc (zipmap (keys g) (repeat inf)) src 0)
          curr      src
          unvisited (disj (apply hash-set (keys g)) src)]
     (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs

       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node  (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))
;; END NOT MY CODE

(defn get-cost-between [p1 p2]
  (if (not= (last p1) (last p2)) 1001 1))
(defn get-points-around [[x y]]
  (let [points-around (set (points/cardinal-points-around [x y]))
        directions-around (map #(conj % (get-direction [x y] %)) points-around)]
    (remove (fn [[x y d]] (contains? walls [x y])) directions-around)))

(defn get-directions-around [[x y]]
  (let [points-around (set (points/cardinal-points-around [x y]))
        directions-around (map #(get-direction [x y] %) (set/intersection walls points-around))]
    directions-around))

(defn get-graph-costs []
  (let [direction-points (mapcat #(map (fn [direction] (conj % direction)) (get-directions-around %)) (set/union spaces #{end}))
        direction-points (conj direction-points (conj start :right))]
    (->> (map #(let [points-around (get-points-around %)]
                 (println "points-around" % points-around)
                 {% (reduce (fn [acc p] (assoc acc p (get-cost-between % p)))
                            {}
                            (set/intersection (set direction-points) (set points-around)))})
              direction-points)
         (apply merge))))




































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