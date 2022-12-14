(ns advent-of-code.2022.day-13
  (:require [advent-of-code.shared.read-file :as read]))

(def test-data (->> [[1, 1, 3, 1, 1]
                     [1, 1, 5, 1, 1]
                     [[1], [2, 3, 4]]
                     [[1], 4]
                     [9]
                     [[8, 7, 6]]
                     [[4, 4], 4, 4]
                     [[4, 4], 4, 4, 4]
                     [7, 7, 7, 7]
                     [7, 7, 7]
                     []
                     [3]
                     [[[]]]
                     [[]]
                     [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
                     [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]]
                    (partition 2)))

(def test-data-part-2 (->> [[[2]]
                            [[6]]
                            [1, 1, 3, 1, 1]
                            [1, 1, 5, 1, 1]
                            [[1], [2, 3, 4]]
                            [[1], 4]
                            [9]
                            [[8, 7, 6]]
                            [[4, 4], 4, 4]
                            [[4, 4], 4, 4, 4]
                            [7, 7, 7, 7]
                            [7, 7, 7]
                            []
                            [3]
                            [[[]]]
                            [[]]
                            [1, [2, [3, [4, [5, 6, 7]]]], 8, 9]
                            [1, [2, [3, [4, [5, 6, 0]]]], 8, 9]]))

(def data (->> (read/load-edn "2022/day_13.edn")
               (partition 2)))

(def data-part-2 (->> (read/load-edn "2022/day_13_part_2.edn")))

;; true true false true false true false false

(defn compare-lr-col [l r]
  (cond
    (and (nil? l) (not (nil? r)))
    -1

    (and (not (nil? l)) (nil? r))
    1

    (and (number? l) (number? r))
    (cond (< l r) -1
          (> l r) 1)

    (and (coll? l) (coll? r))
    ;; Stole this from Charlie
    ;; The (range 0 (max (count l) (count r))) was the secret sauce I was missing to compare the
    ;; two collections of different sizes
    (some (fn [i] (compare-lr-col (get l i) (get r i))) (range 0 (max (count l) (count r))))

    (and (coll? l) (not (coll? r)))
    (compare-lr-col l [r])

    (and (not (coll? l)) (coll? r))
    (compare-lr-col [l] r)))

(defn part-1 []
  (->> (map #(compare-lr-col (first %) (second %)) data)
       (map-indexed #(list (inc %1) %2))
       (filter #(= 1 (second %)))
       (map first)
       (reduce +)))

(defn part-2 []
  (->> (sort compare-lr-col data-part-2)
       (map-indexed #(list (inc %1) %2))
       (filter #(contains? #{[[2]] [[6]]} (second %)))
       (map first)
       (reduce *)))
