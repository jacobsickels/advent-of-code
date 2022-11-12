(ns advent-of-code.2018.day-11
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code.shared.point :as points]))

(def grid (combo/cartesian-product (range 1 301) (range 1 301)))

(defn generate-valid-grid [size]
  (combo/cartesian-product (range 1 (inc (Math/max (- 301 size) 1)))
                           (range 1 (inc (Math/max (- 301 size) 1)))))


(defn digits [n]
  (if (pos? n)
    (conj (digits (quot n 10)) (mod n 10))
    []))

(defn find-power-level [point serial]
  (let [rack-id (+ (first point) 10)
        power (* rack-id (+ serial (* rack-id (second point))))
        hundreds (first (drop 2 (reverse (digits power))))
        hundreds (if (number? hundreds) hundreds 0)]
    (- hundreds 5)))

(defn get-square [[x y] size]
  (combo/cartesian-product (range x (+ x size)) (range y (+ y size))))

(defn get-power [cells points]
  (reduce (fn [acc p]
            (+ acc (or (get cells p) 0)))
          0
          points))

(defn get-max-power [cells size]
  (loop [checking (generate-valid-grid size)
         point []
         largest-power 0]
    (if (empty? checking)
      {:size size :point point :power largest-power}
      (let [square (get-square (first checking) size)
            found-power (get-power cells square)]
        (if (> found-power largest-power)
          (recur (rest checking)
                 (first checking)
                 found-power)
          (recur (rest checking)
                 point
                 largest-power))))))

(defn part-1 [serial]
  (let [cells (reduce (fn [acc point]
                        (assoc acc point (find-power-level point serial)))
                      {}
                      grid)]
    (get-max-power cells 3)))

(defn part-2 [serial]
  (let [cells (reduce (fn [acc point]
                        (assoc acc point (find-power-level point serial)))
                      {}
                      grid)]
    (loop [check-sizes (range 3 301)
           found []]
      (println (first check-sizes) (last found))
      (if (empty? check-sizes)
        found
        (recur (rest check-sizes)
               (conj found (get-max-power cells (first check-sizes))))))))

;; Part-2 is too slow to solve in any meaningful amount of time
;; 233,288,12 was apparently the right answer, the powers spiked early in my dataset with that
;; this was pretty much a guess because I didn't want to wait for the computation to finish