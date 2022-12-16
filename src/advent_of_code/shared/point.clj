(ns advent-of-code.shared.point
  (:require [clojure.math.combinatorics :as combo]))

(defn points-around [point]
  (remove (fn [p] (= point p))
          (apply combo/cartesian-product
                 (map #(range (dec %) (+ 2 %)) point))))

(defn points-around-inclusive [point]
  (sort-by second (apply combo/cartesian-product
                         (map #(range (dec %) (+ 2 %)) point))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))