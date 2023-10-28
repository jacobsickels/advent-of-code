(ns advent-of-code.2015.day-18
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.point :as point]
            [clojure.math.combinatorics :as combo]))

(def test-data (->> [".#.#.#" "...##." "#....#" "..#..." "#.#..#" "####.."]
                    (map #(map str %))))

(def data (->> (read/read-file "resources/2015/day_18.txt")
               (map #(map str %))))

(defn print-state [col]
  (map #(apply str %) col))

(defn get-light [col [x y] corners]
  (if (contains? corners [x y])
    "#"
    (nth (nth col y nil) x nil)))

(defn is-light-on? [col [x y] corners]
  (= "#" (get-light col [x y] corners)))

(defn get-lookup-points [x y]
  (combo/cartesian-product (range 0 x) (range 0 y)))

(defn is-light-on-next-round? [col [x y] corners]
  (let [self-on? (or (is-light-on? col [x y] corners))
        points-around (point/points-around [x y])
        number-neighbors-on (->> (map #(get-light col % corners) points-around)
                                 (map #(= "#" %))
                                 (filter true?)
                                 count)]
    (if (contains? corners [x y])
      true
      (if self-on?
        (or (= 2 number-neighbors-on) (= 3 number-neighbors-on))
        (= 3 number-neighbors-on)))))

(defn get-next-state [col corners]
  (let [width (count (nth col 0))
        height (count col)
        lookup-points (sort-by second (get-lookup-points width height))]
    (->> (map #(if (is-light-on-next-round? col % corners) "#" ".") lookup-points)
         (partition width))))

(defn get-final-state [col final-step corners]
  (loop [state col
         step 0]
    (if (= step final-step)
      (->> (apply concat state)
           (filter #(= "#" %))
           count)
      (recur (get-next-state state corners) (inc step)))))

(defn part-1 [col final-step]                               ;; 814
  (get-final-state col final-step #{}))

(defn part-2 [col final-step]                               ;; 924
  (let [width (count (nth col 0))
        height (count col)
        corners (set [[0 0] [0 (dec width)] [(dec height) 0] [(dec width) (dec height)]])]
    (get-final-state col final-step corners)))
