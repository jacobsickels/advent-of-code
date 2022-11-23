(ns advent-of-code.2016.day-01
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))


(def turn-left {\N \W, \W \S, \S \E, \E \N})
(def turn-right {\N \E, \E \S, \S \W, \W \N})

(def data (-> (read/read-file "resources/2016/day_01.txt")
              first
              (str/split #", ")))

(defn walk-direction [direction [x y] distance]
  (cond
    (= direction \N)
    [x (+ y distance)]

    (= direction \E)
    [(+ x distance) y]

    (= direction \S)
    [x (- y distance)]

    (= direction \W)
    [(- x distance) y]))

(defn part-1 []
  (loop [col data
         direction \N
         point [0 0]]
    (println (first data))
    (if (empty? col)
      (+ (Math/abs (first point)) (Math/abs (second point)))
      (cond
        (= (ffirst col) \R)
        (recur (rest col)
               (get turn-right direction)
               (walk-direction (get turn-right direction) point (->> (first col)
                                                                     rest
                                                                     (apply str)
                                                                     Integer/parseInt)))

        (= (ffirst col) \L)
        (recur (rest col)
               (get turn-left direction)
               (walk-direction (get turn-left direction) point (->> (first col)
                                                                    rest
                                                                    (apply str)
                                                                    Integer/parseInt)))))))

(defn get-manhattan-points-between [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2)
    (let [[yS yL] (sort [y1 y2])
          points (map vector (repeat (- (inc yL) yS) x1) (range yS (inc yL)))]
      (if (> y1 y2)
        (reverse points)
        points))

    (= y1 y2)
    (let [[xS xL] (sort [x1 x2])
          points (map vector (range xS (inc xL)) (repeat (- (inc xL) xS) y1))]
      (if (> x1 x2)
        (reverse points)
        points))))


(defn contains-duplicate-point? [col]
  (let [two-or-more (->> (frequencies col)
                         (filter #(>= (second %) 2)))]
    [(not (zero? (count two-or-more))) (ffirst two-or-more)]))

(defn part-2 []
  (loop [col data
         direction \N
         line [[0 0]]
         acc []]
    (if (first (contains-duplicate-point? acc))
      (second (contains-duplicate-point? acc))
      (if (empty? col)
        (contains-duplicate-point? acc)
        (cond
          (= (ffirst col) \R)
          (let [new-point (walk-direction (get turn-right direction)
                                          (last line)
                                          (->> (first col)
                                               rest
                                               (apply str)
                                               Integer/parseInt))
                points-between (get-manhattan-points-between (last line) new-point)]
            (recur (rest col)
                   (get turn-right direction)
                   points-between
                   (concat acc (butlast points-between))))

          (= (ffirst col) \L)
          (let [new-point (walk-direction (get turn-left direction)
                                          (last line)
                                          (->> (first col)
                                               rest
                                               (apply str)
                                               Integer/parseInt))
                points-between (get-manhattan-points-between (last line) new-point)]
            (recur (rest col)
                   (get turn-left direction)
                   points-between
                   (concat acc (butlast points-between)))))))))