(ns advent-of-code.2023.day-18
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (read/read-file "resources/2023/day_18.txt"))

(def directions (->> (map #(str/split % #" ") data)))

(defn get-next-point [[x y] dir amt [previous-right-turn? next-right-turn?]]
  (cond
    (= dir "U")
    (cond
      (and previous-right-turn? next-right-turn?)
      [x (- y (inc amt))]

      (and (false? previous-right-turn?) (false? next-right-turn?))
      [x (- y (dec amt))]

      (and previous-right-turn? (false? next-right-turn?))
      [x (- y amt)]

      (and (false? previous-right-turn?) next-right-turn?)
      [x (- y amt)] ;; Not sure

      :else (do
              (println "check me U" [x y] dir amt [previous-right-turn? next-right-turn?])
              [x (- y amt)]))

    (= dir "D")
    (cond
      (and previous-right-turn? next-right-turn?)
      [x (+ y (inc amt))]

      (and (false? previous-right-turn?) (false? next-right-turn?))
      [x (+ y (dec amt))]

      (and previous-right-turn? (false? next-right-turn?))
      [x (+ y amt)]

      (and (false? previous-right-turn?) next-right-turn?)
      [x (+ y amt)]

      :else (do
              (println "check me D" [x y] dir amt [previous-right-turn? next-right-turn?])
              [x (+ y amt)]))



    (= dir "L")
    (cond
      (and previous-right-turn? next-right-turn?)
      [(- x (inc amt)) y]

      (and (false? previous-right-turn?) (false? next-right-turn?))
      [(- x (dec amt)) y]

      (and previous-right-turn? (false? next-right-turn?))
      [(- x amt) y]

      (and (false? previous-right-turn?) next-right-turn?)
      [(- x amt) y]

      :else (do
              (println "check me L" [x y] dir amt [previous-right-turn? next-right-turn?])
              [(- x amt) y]))



    (= dir "R")
    (cond
      (and previous-right-turn? next-right-turn?)
      [(+ x (inc amt)) y]

      (and (false? previous-right-turn?) (false? next-right-turn?))
      [(+ x (dec amt)) y]

      (and previous-right-turn? (false? next-right-turn?))
      [(+ x amt) y]

      (and (false? previous-right-turn?) next-right-turn?)
      [(+ x amt) y]

      :else (do
              (println "check me R" [x y] dir amt [previous-right-turn? next-right-turn?])
              [(+ x amt) y]))))

(defn is-next-right-turn? [[prev-direction direction]]
  #_(println prev-direction direction)
  (cond
    (and (= prev-direction "R") (= direction "D")) true
    (and (= prev-direction "R") (= direction "U")) false

    (and (= prev-direction "L") (= direction "D")) false
    (and (= prev-direction "L") (= direction "U")) true

    (and (= prev-direction "D") (= direction "L")) true
    (and (= prev-direction "D") (= direction "R")) false

    (and (= prev-direction "U") (= direction "L")) false
    (and (= prev-direction "U") (= direction "R")) true))

(defn get-turns [dirs]
  (->> (map first dirs)
       (partition 2 1)
       (map is-next-right-turn?)))

(defn get-border []
  (loop [d directions
         turns (get-turns directions)
         results []
         point [1 1]]
    (if (empty? d)
      results
      (let [[dir num-str] (first d)
            new-points (points/points-between point (get-next-point point dir (Integer/parseInt num-str) (first turns)))
            new-points (if (contains? #{"U" "L"} dir) (reverse new-points) new-points)]
        (recur (rest d) (rest turns) (concat results (butlast new-points)) (last new-points))))))


(defn get-border-ranges []
  (let [turns (concat [true] (get-turns directions) [true])]
    (loop [d directions
           turn-index 0
           results []
           point [0 0]]
      (if (empty? d)
        results
        (let [[dir num-str] (first d)
              new-points [point (get-next-point point dir (Integer/parseInt num-str) (take 2 (drop turn-index turns)))]]
          #_(println dir num-str new-points (take 2 (drop turn-index turns)))
          (recur (rest d)
                 (inc turn-index)
                 (conj results new-points)
                 (last new-points)))))))

(defn get-border-ranges []
  (let [turns (concat [true] (get-turns directions) [true])]
    (loop [d directions
           turn-index 0
           results []
           point [0 0]]
      (if (empty? d)
        results
        (let [[dir num-str] (first d)
              new-points [point (get-next-point point dir (Integer/parseInt num-str) (take 2 (drop turn-index turns)))]]
          #_(println dir num-str new-points (take 2 (drop turn-index turns)))
          (recur (rest d)
                 (inc turn-index)
                 (conj results new-points)
                 (last new-points)))))))

(defn do-math [[[x1 y1] [x2 y2]]]
  (- (* x1 y2) (* y1 x2)))

(defn part-1 []
  (loop [ranges (map (fn [[r1 r2]] [(map inc r1) (map inc r2)]) (get-border-ranges))
         upper []]
    (if (empty? ranges)
      (abs (/ (reduce + upper) 2))
      (recur (rest ranges) (conj upper (do-math (first ranges)))))))

(def direction-from-num {\0 "R" \1 "D" \2 "L" \3 "U"})


(defn parse-part-2-hash [hex]
  (let [hex-col (butlast (drop 2 hex))
        dir (get direction-from-num (last hex-col))
        num-str (apply str (butlast hex-col))]
    [dir (Integer/parseInt num-str 16)]))

(def part-2-data
  (->> (map last directions)
       (map parse-part-2-hash)))

(defn get-border-ranges-2 []
  ;; (concat [false] (get-turns part-2-data) [false])
  ;; This represents the wrap around turn that we need to check,
  ;; If first command is R and last is U then they are [true] [true]
  ;; My data had L and last U so [false] [false]
  ;; for example data it is
  ;; (concat [true] (get-turns part-2-data) [true])
  (let [turns (concat [false] (get-turns part-2-data) [false])]
    (loop [d directions
           turn-index 0
           results []
           point [0 0]]
      (if (empty? d)
        results
        (let [[_ _ hex] (first d)
              hex-col (butlast (drop 2 hex))
              dir (get direction-from-num (last hex-col))
              num-str (apply str (butlast hex-col))
              new-points [point (get-next-point point dir (Integer/parseInt num-str 16) (take 2 (drop turn-index turns)))]]
          (recur (rest d)
                 (inc turn-index)
                 (conj results new-points)
                 (last new-points)))))))

(defn part-2 []
  (loop [ranges (map (fn [[r1 r2]] [(map inc r1) (map inc r2)]) (get-border-ranges-2))
         upper []]
    (if (empty? ranges)
      (abs (/ (reduce + upper) 2))
      (recur (rest ranges) (conj upper (do-math (first ranges)))))))