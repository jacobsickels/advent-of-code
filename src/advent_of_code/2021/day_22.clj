(ns advent-of-code.2021.day-22
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [advent-of-code.shared.read-file :as read]))

(def data (read/read-file "resources/day_22.txt"))

(def line "on x=10..12,y=10..12,z=10..12")

(defn format-line [line]
  (let [[action area] (str/split line #" ")
        [x y z] (str/split area #",")
        find-coords (fn [coord-str] (str/split coord-str #"[=|..]"))
        [_ x1 _ x2] (find-coords x)
        [_ y1 _ y2] (find-coords y)
        [_ z1 _ z2] (find-coords z)]
    {:action action 
     :x (map #(Integer/parseInt %) [x1 x2])
     :y (map #(Integer/parseInt %) [y1 y2])
     :z (map #(Integer/parseInt %) [z1 z2])}))

(defn get-cube-points [action-area]
  (let [[x1 x2] (:x action-area)
        [y1 y2] (:y action-area)
        [z1 z2] (:z action-area)]
    (set 
      (apply
        combo/cartesian-product
        [(core/inclusive-range x1 x2)
         (core/inclusive-range y1 y2)
         (core/inclusive-range z1 z2)]))))

(defn get-part-1-data []
  (->> (map format-line data)
       (filter (fn [action-area]
                 (and 
                   (<= -50 (first (:x action-area)) (second (:x action-area)) 50)
                   (<= -50 (first (:y action-area)) (second (:y action-area)) 50)
                   (<= -50 (first (:z action-area)) (second (:z action-area)) 50))))))

(defn part-1 []
  (loop [actions (get-part-1-data)
         on-points #{}]
    (if (empty? actions)
      (count on-points)
      (if (= "on" (:action (first actions)))
        (recur (rest actions) (set/union on-points (get-cube-points (first actions))))
        (recur (rest actions) (set/difference on-points (get-cube-points (first actions))))))))
       