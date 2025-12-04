(ns advent-of-code.2025.day-04
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2025/day_04.txt")))

(def points (->> (map #(str/split % #"") data)
                 (map-indexed (fn [y line]
                                (map-indexed (fn [x space] [space x y]) line)))
                 (apply concat)))

(def paper-points (->> (filter #(= "@" (first %)) points)
                       (map (fn [[p x y]] [x y]))
                       set))

(defn is-removable-paper-point? [coll point]
  (let [points-around (set (points/points-around point))]
    (and (>= 3 (count (set/intersection coll points-around)))
         (contains? coll point))))

(defn part-1 []
  (->> (map #(is-removable-paper-point? paper-points %) paper-points)
       (frequencies)))


(defn get-all-removable-papers [paper-set]
  (set (filter #(is-removable-paper-point? paper-set %) paper-set)))

(defn part-2 []
  (loop [papers paper-points]
    (let [removable-papers (get-all-removable-papers papers)]
      (if (empty? removable-papers)
        (- (count paper-points) (count papers))
        (recur (set/difference papers removable-papers))))))