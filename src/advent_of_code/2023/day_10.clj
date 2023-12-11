(ns advent-of-code.2023.day-10
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.point :as point]
            [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2023/day_10.txt")))

(def starting-point (let [[y col] (->> (map-indexed (fn [i col] (if (str/includes? col "S") [i col] [i nil])) data)
                                       (filter #(not (nil? (second %))))
                                       first)]
                      [(str/index-of col "S") y]))

(defn get-point-character [[x y]]
  (nth (nth data y []) x \.))

(defn assoc-point-character [acc [x y]]
  (assoc acc [x y] (get-point-character [x y])))

(defn is-point-below? [[x y] [cx cy]]
  (and (= x cx) (= (inc y) cy)))

(defn is-point-above? [[x y] [cx cy]]
  (and (= x cx) (= (dec y) cy)))

(defn is-point-left? [[x y] [cx cy]]
  (and (= (dec x) cx) (= y cy)))

(defn is-point-right? [[x y] [cx cy]]
  (and (= (inc x) cx) (= y cy)))

(defn get-direction-cares [character]
  (get {\| #{:north :south}
        \- #{:east :west}
        \L #{:north :east}
        \J #{:north :west}
        \7 #{:south :west}
        \F #{:south :east}
        \S #{:north :south :east :west}} character))

(defn get-valid-point-around [pipe]
  (let [point (last pipe)
        points-around (point/cardinal-points-around point)
        characters-around (reduce assoc-point-character {} points-around)
        direction-cares (get-direction-cares (get-point-character point))
        last-2 (take-last 2 pipe)
        found-points (->> (map (fn [[k v]]
                                 (cond
                                   (and (contains? direction-cares :east) (is-point-right? point k) (contains? #{\- \7 \J} v))
                                   k

                                   (and (contains? direction-cares :south) (is-point-below? point k) (contains? #{\| \L \J} v))
                                   k

                                   (and (contains? direction-cares :west) (is-point-left? point k) (contains? #{\- \L \F} v))
                                   k

                                   (and (contains? direction-cares :north) (is-point-above? point k) (contains? #{\| \F \7} v))
                                   k

                                   :else nil))
                               characters-around))]
    (->> (remove #(or (nil? %) (contains? (set last-2) %)) found-points)
         first)))


(defn find-pipe []
  (loop [results [starting-point]]
    (let [next-valid-point (get-valid-point-around results)]
      (if (nil? next-valid-point)
        results
        (recur (conj results next-valid-point))))))

(defn part-1 []
  (/ (count (find-pipe)) 2))

(defn get-most-top-left [pipe]
  (->> (sort-by second pipe)
       (partition-by second)
       first
       (sort-by first)
       first))

(defn remove-pipe-points [inside-points pipe-set]
  (set/difference (set inside-points) pipe-set))

(defn get-next-point-from-normal [normal [x y]]
  (cond
    (= normal :south) [x (inc y)]
    (= normal :west)  [(dec x) y]
    (= normal :east)  [(inc x) y]
    (= normal :north) [x (dec y)]))

(defn get-next-points
  "When the next character is a corner we need to give back two points as the insides"
  [direction normal [x y]]
  (let [next-point-character (get-point-character [x y])]
    (cond
      (and (= direction :east) (= next-point-character \J))
      [[x (inc y)] [(inc x) y]]

      (and (= direction :west) (= next-point-character \F))
      [[(dec x) y] [x (dec y)]]

      (and (= direction :north) (= next-point-character \7))
      [[(inc x) y] [x (dec y)]]

      (and (= direction :south) (= next-point-character \L))
      [[(dec x) y] [x (inc y)]]

      (and (= direction :east) (= next-point-character \7))
      []

      (and (= direction :west) (= next-point-character \L))
      []

      (and (= direction :north) (= next-point-character \F))
      []

      (and (= direction :south) (= next-point-character \J))
      []

      :else [(get-next-point-from-normal normal [x y])])))


(defn find-first-inner-points [pipe-col]
  (let [pipe-set (set pipe-col)]
    (loop [position (get-most-top-left pipe-col)
           position-index (first (utils/index-of position pipe-col))
           inside-points []
           iterations 0]
      (if (= iterations (count pipe-col))
        (remove-pipe-points inside-points pipe-set)
        (let [next-position (nth pipe-col (mod (inc position-index) (count pipe-col)))]
          (cond
            (and (is-point-right? position next-position))
            (recur next-position
                   (inc position-index)
                   (apply conj inside-points (get-next-points :east :south next-position))
                   (inc iterations))

            (and (is-point-below? position next-position))
            (recur next-position
                   (inc position-index)
                   (apply conj inside-points (get-next-points :south :west next-position))
                   (inc iterations))

            (and (is-point-above? position next-position))
            (recur next-position
                   (inc position-index)
                   (apply conj inside-points (get-next-points :north :east next-position))
                   (inc iterations))

            (and (is-point-left? position next-position))
            (recur next-position
                   (inc position-index)
                   (apply conj inside-points (get-next-points :west :north next-position))
                   (inc iterations))

            :else (recur next-position (inc position-index) inside-points (inc iterations))))))))

(defn flood-fill [pipe-col inner-points]
  (let [pipe-set (set pipe-col)]
    (loop [result inner-points]
      (let [new-points (set/union result (set/difference (set (mapcat points/cardinal-points-around result)) pipe-set))]
        (if (= result new-points)
          result
          (recur new-points))))))

;(defn get-points-around-field []
;  (let [width (count (nth data 0))
;        height (count data)
;        field (combo/cartesian-product (range -1 (inc width)) (range -1 (inc height)))]
;    (set (filter (fn [[x y]]
;                   (or (neg? x) (neg? y) (= x width) (= y height)))
;                 field))))

(defn part-2 []
  (let [pipe-col (find-pipe)
        first-inner-points (find-first-inner-points pipe-col)
        is-pipe-clockwise? (->> (filter #(or (neg? (first %)) (neg? (second %))) first-inner-points)
                                count
                                zero?)
        first-inner-points (if is-pipe-clockwise? first-inner-points (find-first-inner-points (reverse pipe-col)))
        pipe-col (if is-pipe-clockwise? pipe-col (reverse pipe-col))]
    (count (flood-fill pipe-col first-inner-points))))

