(ns advent-of-code.2021.day-9
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def data 
  (mapv vec (mapv (fn [s] (map #(Integer/parseInt %) (str/split s #"")))
                  (read/read-file "resources/day_09.txt"))))

(def test-data [[2 1 9 9 9 4 3 2 1 0]
                [3 9 8 7 8 9 4 9 2 1]
                [9 8 5 6 7 8 9 8 9 2]
                [8 7 6 7 8 9 6 7 8 9]
                [9 8 9 9 9 6 5 6 7 8]])

(defn points-around [[x y] width height]
  (remove empty? (list (if (= (dec x) -1) nil [(dec x) y])
                       (if (>= (inc x) width) nil [(inc x) y])
                       (if (= (dec y) -1) nil [x (dec y)])
                       (if (>= (inc y) height) nil [x (inc y)]))))

(defn is-low-point? [[x y] input]
  (let [width (count (first input))
        height (count input)
        around (points-around [x y] width height)
        check (get (get input y) x)]
    (< check (first (sort (map (fn [[xA yA]] (get (get input yA) xA)) 
                               around))))))

(defn get-low-points [input]
  (let [width (count (first input))
        height (count input)
        allowed-points (apply concat (map (fn [h]
                                            (map (fn [w] (list w h))
                                                 (range 0 width)))
                                          (range 0 height)))]
    (filter 
      #(is-low-point? % input)
      allowed-points)))

(defn part-1 [input]
  (reduce + (map (fn [[x y]] (inc (get (get input y) x))) 
                 (get-low-points input))))


;; Part 2
(defn points-around-in-basin [[x y] input found]
  (let [width  (count (first input))
        height (count input)
        around (points-around [x y] width height)]
    (set (conj (filter
                 (fn [[xA yA]]
                   (let [value (get (get input yA) xA)]
                     (and
                       (not (contains? found [xA yA]))
                       (not= value 9))))
                 around)
               [x y]))))

(defn get-basin [[x y] input]
  (loop [found (points-around-in-basin [x y] input #{[x y]})
         last-found nil]
    (if (= found last-found)
      found
      (let [next (set (apply concat (map #(points-around-in-basin % input found) found)))]
        (recur next found)))))

(defn part-2 [input]
  (->>
    (get-low-points input)
    (map #(get-basin % input))
    (sort-by count)
    (take-last 3)
    (map count)
    (reduce *)))