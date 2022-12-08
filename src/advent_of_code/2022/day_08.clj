(ns advent-of-code.2022.day-08
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn parse-int-col [col]
  (map #(Integer/parseInt %) col))

(def data (->> (read/read-file "resources/2022/day_08_example.txt")
               (map #(str/split % #""))
               (map parse-int-col)))

(defn get-to-edges [trees [x y]]
  (let [width (count (first trees))
        height (count trees)
        top (map #(list %1 %2) (repeat (inc y) x) (range 0 (inc y)))
        left (map #(list %1 %2) (range 0 (inc x)) (repeat (inc x) y))
        right (map #(list %1 %2) (range  x (inc width)) (repeat (-  width x) y))
        bottom (map #(list %1 %2) (repeat (- height y) x) (range y (inc height)))]
    {:top (map (fn [[x y]] (nth (nth trees y) x)) top)
     :left (map (fn [[x y]] (nth (nth trees y) x)) left)
     :right (map (fn [[x y]] (nth (nth trees y) x)) (reverse right))
     :bottom (map (fn [[x y]] (nth (nth trees y) x)) (reverse bottom))}))

(defn is-visible? [trees [x y]]
  (let [edges (get-to-edges trees [x y])]
    [[x y]
     (or (< (apply max (butlast (:top edges))) (last (:top edges)))
         (< (apply max (butlast (:right edges))) (last (:right edges)))
         (< (apply max (butlast (:left edges))) (last (:left edges)))
         (< (apply max (butlast (:bottom edges))) (last (:bottom edges))))]))

(defn get-outer-points [width height]
  (->> (combo/cartesian-product (range 0 width) (range 0 height))
       (filter (fn [[x y]] (or (= x 0)
                               (= y 0)
                               (= x (dec width))
                               (= y (dec height)))))))

(defn part-1 [trees]
  (let [width (count (first trees))
        height (count trees)
        inside-points (combo/cartesian-product (range 1 (dec width)) (range 1 (dec height)))
        outer-points (get-outer-points width height)
        cnt-visible-inside (->> (map #(is-visible? trees %) inside-points)
                                (filter #(true? (second %)))
                                count)]
    (+ (count outer-points) cnt-visible-inside)))

(defn visible-from-list [col self-height]
  (loop [checks (reverse (butlast col))
         acc []]
    (if (empty? checks)
      acc
      (cond
        (> self-height (first checks))
        (recur (rest checks) (conj acc (first checks)))

        (<= self-height (first checks))
        (conj acc (first checks))))))

(defn get-scenic-score [trees [x y]]
  (let [edges (get-to-edges trees [x y])
        self-height (last (:top edges))]
    (* (count (visible-from-list (:top edges) self-height))
       (count (visible-from-list (:right edges) self-height))
       (count (visible-from-list (:left edges) self-height))
       (count (visible-from-list (:bottom edges) self-height)))))

(defn part-2 [trees]
  (let [width (count (first trees))
        height (count trees)
        inside-points (combo/cartesian-product (range 1 (dec width)) (range 1 (dec height)))]
    (->> (map #(list % (get-scenic-score trees %)) inside-points)
         (sort-by second)
         last
         last)))