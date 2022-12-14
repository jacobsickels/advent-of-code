(ns advent-of-code.2021.day-15
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]
            [com.rpl.specter :as S]
            [clojure.math.combinatorics :as combo]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.java.io :as io]
            [advent-of-code.shared.read-file :as read]))

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

(defn make-dijkstra-graph [input]
  (let [width  (count (first input))
        height (count input)]
    (reduce
      (fn [acc p] (conj acc {p (reduce
                                 (fn [accI pI]
                                   (conj accI {pI (get-point-weight pI input)}))
                                 {}
                                 (points-around p))}))
      {}
      (core/point-grid width height false))))


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

(defn part-1 [input]
  (let [width  (dec (count (first input)))
        height (dec (count input))]
    (dijkstra (make-dijkstra-graph input) '(0 0) [width height])))


;; Part 2
(defn get-point-weight-2 [[x y] input]
  (let [width  (count (first input))
        height (count input)]
    (when (not (or (neg? x)
                   (neg? y)
                   (>= x (* width 5))
                   (>= y (* height 5))))
      (nth (nth input (mod y height)) (mod x width)))))


(defn make-dijkstra-graph-2 [input]
  (let [width       (count (first input))
        full-width  (dec (* 5 width))
        height      (count input)
        full-height (dec (* 5 height))]
    (reduce
      (fn [acc p] (conj acc {p (reduce
                                 (fn [accI pI]
                                   (conj accI {pI
                                               (let [found (get-point-weight-2 pI input)
                                                     incced (when found (+ found
                                                                           (quot (first pI) width)
                                                                           (quot (second pI) height)))]
                                                 (if (and incced (> incced 9)) 
                                                   (- incced 9) incced))}))
                                                 
                                 {}
                                 (points-around p))}))
      {}
      (core/point-grid full-width full-height true))))

(defn part-2 [input]
  (let [width       (count (first input))
        full-width  (dec (* 5 width))
        height      (count input)
        full-height (dec (* 5 height))]
    (println full-width full-height)
    (dijkstra (make-dijkstra-graph-2 input) '(0 0) [full-width full-height])))