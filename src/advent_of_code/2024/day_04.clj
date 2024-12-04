(ns advent-of-code.2024.day-04
  (:require [advent-of-code.shared.point :as point]
            [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]))

(def data (read/read-file "resources/2024/day_04.txt"))

(defn generate-points-around [[x y]]
  (list [[x y] [x (+ y 1)] [x (+ y 2)] [x (+ y 3)]]   ;; down
        [[x y] [x (- y 1)] [x (- y 2)] [x (- y 3)]]   ;; up
        [[x y] [(- x 1) y] [(- x 2) y] [(- x 3) y]]   ;; left
        [[x y] [(+ x 1) y] [(+ x 2) y] [(+ x 3) y]]   ;; right
        [[x y] [(+ x 1) (- y 1)] [(+ x 2) (- y 2)] [(+ x 3) (- y 3)]]   ;; ne
        [[x y] [(+ x 1) (+ y 1)] [(+ x 2) (+ y 2)] [(+ x 3) (+ y 3)]]   ;; se
        [[x y] [(- x 1) (+ y 1)] [(- x 2) (+ y 2)] [(- x 3) (+ y 3)]]   ;; sw
        [[x y] [(- x 1) (- y 1)] [(- x 2) (- y 2)] [(- x 3) (- y 3)]])) ;; nw

(defn get-char [[x y]]
  (get (get data y) x))

(defn get-points-string [line-points]
  (->> (map get-char line-points)
       (apply str)))

(defn xmas-point-count [[x y]]
  (->> (generate-points-around [x y])
       (map get-points-string)
       (filter #(= "XMAS" %))
       count))

(defn part-1 []
  (->> (map xmas-point-count (point/get-all-points data))
       (reduce +)))

(defn generate-points-around-x [[x y]]
  (list [[(dec x) (dec y)] [x y] [(inc x) (inc y)]]
        [[(dec x) (inc y)] [x y] [(inc x) (dec y)]]))

(defn is-x-point? [[x y]]
  (empty? (set/difference (->> (generate-points-around-x [x y])
                               (map get-points-string)
                               set)
                          #{"MAS" "SAM"})))

(defn part-2 []
  (get (->> (map is-x-point? (point/get-all-points data))
            frequencies) true))