(ns advent-of-code.2016.day-08
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]))

(def board (map (fn [_] (repeat 50 " ")) (range 0 6)))

(defn format-data [col]
  (map
    (fn [l] (remove (fn [d] (= d "by")) l))
    (map #(str/split % #" ") col)))

(defn view-board [col]
  (map #(apply str %) col))

(defn rect [x y]
  (combo/cartesian-product (range 0 x) (range 0 y)))

(defn update-point-on-board [board [x y] active?]
  (assoc (vec board) y (assoc (vec (nth board y)) x (if active? "#" " "))))

(defn apply-rect-to-board [board [x y]]
  (loop [points (rect x y)
         b board]
    (if (empty? points)
      b
      (recur (rest points) (update-point-on-board b (first points) true)))))

(defn rotate-column [board column amt]
  (let [c (map #(nth % column) board)]
    (map-indexed
      (fn [idx a] (list a [column idx]))
      (flatten (reverse [(take (- 6 amt) c) (drop (- 6 amt) c)])))))

(defn apply-rotate-column-to-board [board column amt]
  (loop [points (rotate-column board column amt)
         b board]
    (if (empty? points)
      b
      (let [[active-state p] (first points)]
        (recur (rest points) (update-point-on-board b p (= active-state "#")))))))

(defn rotate-row [board row amt]
  (let [r (nth board row)]
    (flatten (reverse [(take (- 50 amt) r) (drop (- 50 amt) r)]))))

(defn apply-rotate-row-to-board [board row amt]
  (assoc (vec board) row (rotate-row board row amt)))

(defn update-board-with-directions []
  (let [board (map (fn [_] (repeat 50 " ")) (range 0 6))
        data (format-data (read/read-file "resources/2016-8.txt"))]
    (loop [b board
           directions data]
      (if (empty? directions)
        b
        (cond
          (= (ffirst directions) "rotate")
          (let [[_ type lane amt] (first directions)
                lane-num (Integer/parseInt (last (str/split lane #"=")))
                amt-num (Integer/parseInt amt)]
            (if (= type "row")
              (recur (apply-rotate-row-to-board b lane-num amt-num) (rest directions))
              (recur (apply-rotate-column-to-board b lane-num amt-num) (rest directions))))

          (= (ffirst directions) "rect")
          (let [[_ rectangle] (first directions)
                [x y] (map #(Integer/parseInt %) (str/split rectangle #"x"))]
            (recur (apply-rect-to-board b [x y]) (rest directions))))))))

(defn day-8 []
  (frequencies (flatten (update-board-with-directions))))

(defn day-8-2 []
  (view-board (update-board-with-directions)))