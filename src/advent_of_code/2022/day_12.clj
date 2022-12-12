(ns advent-of-code.2022.day-12
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2022/day_12.txt")
               (map #(str/split % #""))))

(def start-point (->> (map-indexed (fn [i row]
                                     (map-indexed (fn [j c] (if (= c "S") [j i] nil)) row))
                                   data)
                      (apply concat)
                      (remove nil?)
                      first))

(def end-point (->> (map-indexed (fn [i row]
                                   (map-indexed (fn [j c] (if (= c "E") [j i] nil)) row))
                                 data)
                    (apply concat)
                    (remove nil?)
                    first))

(defn points-around [[x y]]
   (list [(inc x) y]
         [(dec x) y]
         [x (inc y)]
         [x (dec y)]))

(defn valid-points-around [[x y] visited]
  (let [around (->> (points-around [x y])
                    (filter (fn [[x y]] (and (>= x 0)
                                             (>= y 0)
                                             (< x (count (first data)))
                                             (< y (count data)))))
                    (remove (fn [[x y]] (contains? visited [x y]))))
        point-value (int (first (nth (nth data y) x)))
        point-value (if (= [x y] start-point) (int \a) point-value)]
    (->> (map (fn [[x1 y1]] [[x1 y1] (nth (nth data y1) x1)]) around)
         (filter (fn [[p c]] (let [c-value (int (first c))]
                               (if (= p end-point)
                                 (<= (int \z) (inc point-value))
                                 (<= c-value (inc point-value)))))))))

(defn points-to-end [start]
  (loop [paths #{start}
         visited #{start}
         distance 0]
    (if (contains? (set paths) end-point)
      distance
      (if (empty? paths)
        nil
        (let [next-paths (mapcat #(map first (valid-points-around % visited)) paths)]
          (recur (set next-paths)
                 (into visited paths)
                 (inc distance)))))))

(defn part-1 [] (points-to-end start-point)) ;; dec here because I'm counting the start point

(def all-a-points (->> (map-indexed (fn [i row]
                                      (map-indexed (fn [j c] (if (or (= c "S") (= c "a")) [j i] nil)) row))
                                    data)
                       (apply concat)
                       (remove nil?)))

(defn part-2 []
  (loop [check-points all-a-points
         acc []]
    (if (empty? check-points)
      (->> (sort acc)
           (remove nil?)
           first)
      (recur (rest check-points)
             (conj acc (points-to-end (first check-points)))))))