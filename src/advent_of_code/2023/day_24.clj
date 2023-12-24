(ns advent-of-code.2023.day-24
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data (read/read-file "resources/2023/day_24.txt"))

(def hailstones (->> data
                     (map (fn [l] (let [[position velocity] (str/split l #" @ ")]
                                    {:position (->> (str/split (str/trim position) #", ")
                                                    (map #(BigInteger. (str/trim %))))
                                     :velocity (->> (str/split (str/trim velocity) #", ")
                                                    (map #(BigInteger. (str/trim %))))})))))

(defn get-y-intercept [[x y] [dx dy]]
  (- y (* (/ dy dx) x)))

(defn point-slope [slope y-intercept]
  (fn [x] (+ (* slope x) y-intercept)))

(defn find-intersection [stone-1 stone-2]
  (let [[x1 y1] (:position stone-1)
        [dx1 dy1] (:velocity stone-1)
        [x2 y2] (:position stone-2)
        [dx2 dy2] (:velocity stone-2)
        stone-1-y-intercept (get-y-intercept [x1 y1] [dx1 dy1])
        stone-2-y-intercept (get-y-intercept [x2 y2] [dx2 dy2])
        stone-1-point-slope (point-slope (/ dy1 dx1) stone-1-y-intercept)
        stone-1-slope (/ dy1 dx1)
        stone-2-slope (/ dy2 dx2)]
    (if (= stone-1-slope stone-2-slope)
      nil
      (let [x-intersection (/ (- stone-1-y-intercept stone-2-y-intercept) (- (/ dy2 dx2) (/ dy1 dx1)))]
        [x-intersection (stone-1-point-slope x-intersection)]))))

(defn inside-test-area? [intersection-point]
  (let [low 200000000000000
        high 400000000000000]
    (and (<= low (first intersection-point) high) (<= low (second intersection-point) high))))

(defn crossed-in-time-frame? [intersection-point hailstone]
  (let [[xi yi] intersection-point
        [x y] (:position hailstone)
        [dx dy] (:velocity hailstone)
        slope (/ dy dx)]
    (cond
      (and (neg? slope) (pos? dy))
      (and (<= xi x) (<= y yi))

      (and (neg? slope) (neg? dy))
      (and (<= x xi) (<= yi y))

      (and (pos? slope) (pos? dy))
      (and (<= x xi) (<= y yi))

      (and (pos? slope) (neg? dy))
      (and (<= xi x) (<= yi y)))))

(defn part-1 []
  (let [hailstone-pairs (->> (combo/cartesian-product hailstones hailstones)
                             (remove #(= (first %) (second %)))
                             (map set)
                             set)]
    (->> (map (fn [col] (let [intersection (find-intersection (first col) (second col))]
                          [intersection col]))
              hailstone-pairs)
         (remove #(nil? (first %)))
         (filter (fn [col] (inside-test-area? (first col))))
         (map (fn [col]
                (conj col (->> (map #(crossed-in-time-frame? (first col) %) (second col))
                               (every? true?)))))
         (filter (fn [col] (last col)))
         (count))))


