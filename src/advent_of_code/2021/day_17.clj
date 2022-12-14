(ns advent-of-code.2021.day-17
  (:require [advent-of-code-2021.core :as core]))

(def data-area {:x [211 232] :y [-124 -69]})

(def test-area {:x [20 30] :y [-10 -5]})

(defn in-area? [area [x y]]
  (let [[x1 x2] (:x area)
        [y1 y2] (:y area)]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn past-area? [area [x y]]
  (let [[x1 x2] (:x area)
        [y1 y2] (:y area)]
    (or (> x x2) (< y y1))))

(defn apply-velocity [[x y] [dx dy]]
  [[(+ x dx) 
    (+ y dy)] 
   [(if (pos? dx) (dec dx) 0) 
    (dec dy)]])

(defn get-points [[i-dx i-dy] area]
  (loop [[x y] [0 0]
         [dx dy] [i-dx i-dy]
         acc []]
    (cond 
      (past-area? area [x y])
      [false [i-dx i-dy] (conj acc [x y])]
      
      (in-area? area [x y])
      [true [i-dx i-dy] (conj acc [x y])]
      
      :else 
      (let [[new-point new-velocity] (apply-velocity [x y] [dx dy])]
        (recur new-point new-velocity (conj acc [x y]))))))

(defn get-max-height [path]
  (apply max (map second path)))

;; range-x 200
;; range-y 200
;; Not sure how to make sure this range works
;; Answer was 7626
(defn part-1 [input-area range-x range-y]
  (->> (core/point-grid range-x range-y true)
       (map #(get-points % input-area))
       (filter first)
       (mapv #(list (second %) (-> % last get-max-height)))
       (map second)
       (apply max)))

;; I did checks with:
;; -600 600 -600 600 => 2032
;; -400 400 -400 400 => 2032
;; -300 300 -300 300 => 2032

;; However this one was different
;; -200 200 -200 200 => 800

;; So maximum is somewhere between [-200 200] and [-300, 300] for each but Im lazy and brute force isn't too slow
;; Answer was 2032
(defn part-2 [input-area min-x max-x min-y max-y]
  (->> (core/point-grid min-x max-x min-y max-y)
       (map #(get-points % input-area))
       (filter first)
       (mapv #(list (second %) (-> % last get-max-height)))
       count))