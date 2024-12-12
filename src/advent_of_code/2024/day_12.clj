(ns advent-of-code.2024.day-12
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as medley]))

(def data (->> (read/read-file "resources/2024/day_12.txt")))

(def points (->> (map #(str/split % #"") data)
                 (map-indexed (fn [y line]
                                (map-indexed (fn [x space] [space x y]) line)))
                 (apply concat)
                 set))

(defn get-region-point [[x y]]
  [(str (get (get data y) x)) x y])

(defn get-region [region]
  (let [[s _ _] (first region)
        next-points (->> (mapcat (fn [[s x y]] (conj (points/cardinal-points-around [x y]) [x y])) region)
                         (map get-region-point)
                         (remove nil?)
                         (filter #(= s (first %)))
                         set)]
    (if (= next-points region)
      region
      (get-region next-points))))

(defn get-regions []
  (loop [checks points
         regions []]
    (if (empty? checks)
      regions
      (let [region (get-region [(first checks)])]
        (recur (set/difference checks region) (conj regions region))))))

(defn get-edge-count [region]
  (let [[s _ _] (first region)]
    (->> (map (fn [[s x y]] (->> (points/cardinal-points-around [x y])
                                 (map get-region-point))) region)
         (map #(remove (fn [[rs _ _]] (= rs s)) %))
         (map count)
         (reduce +))))

(defn part-1 []
  (let [regions (get-regions)]
    (->> (map #(* (count %) (get-edge-count %)) regions)
         (reduce +))))

(defn is-edge-point? [point]
  (>= 4 (let [[s x y] point
              points-around (points/cardinal-points-around [x y])]
          (->> (map get-region-point points-around)
               (remove (fn [[rs _ _]] (= rs s)))
               count))))

(defn get-edges [region]
  (filter is-edge-point? region))

(defn get-direction-point [[s x y] direction]
  (cond
    (= direction "north") [x (dec y)]
    (= direction "east") [(inc x) y]
    (= direction "south") [x (inc y)]
    (= direction "west") [(dec x) y]))

(defn faces-in-north-south-direction [region direction]
  (->> (map #(get-direction-point % direction) region)
       (map get-region-point)
       (remove #(= (first %) (ffirst region)))
       (map #(drop 1 %))
       (sort-by second)
       (partition-by second)
       (map #(sort-by first %))
       (mapcat #(medley/partition-between (fn [[x1 y1] [x2 y2]] (> x2 (inc x1))) %)) ;; partition contiguously
       count))

(defn faces-in-east-west-direction [region direction]
  (->> (map #(get-direction-point % direction) region)
       (map get-region-point)
       (remove #(= (first %) (ffirst region)))
       (map #(drop 1 %))
       (sort-by first)
       (partition-by first)
       (map #(sort-by second %))
       (mapcat #(medley/partition-between (fn [[x1 y1] [x2 y2]] (> y2 (inc y1))) %)) ;; partition contiguously
       count))

(defn get-faces-for-region [region]
  (+ (faces-in-north-south-direction region "north")
     (faces-in-north-south-direction region "south")
     (faces-in-east-west-direction region "east")
     (faces-in-east-west-direction region "west")))

(defn part-2 []
  (let [regions (get-regions)]
    (->> (map #(* (count %) (get-faces-for-region (get-edges %))) regions)
         (reduce +))))