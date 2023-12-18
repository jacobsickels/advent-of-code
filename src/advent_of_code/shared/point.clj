(ns advent-of-code.shared.point
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code.shared.utils :as utils]))

(defn points-around [point]
  (->> (map #(utils/inclusive-range (dec %) (inc %)) point)
       (apply combo/cartesian-product)
       (remove (fn [p] (= point p)))))

(defn cardinal-points-around [[x y]]
  (list [(inc x) y]
        [(dec x) y]
        [x (inc y)]
        [x (dec y)]))

(defn points-around-inclusive [point]
  (->> (map #(utils/inclusive-range (dec %) (inc %)) point)
       (apply combo/cartesian-product)
       (sort-by second)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn points-between [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2)
    (map #(vec [x1 %]) (apply utils/inclusive-range (sort [y1 y2])))

    (= y1 y2)
    (map #(vec [% y1]) (apply utils/inclusive-range (sort [x1 x2])))))
