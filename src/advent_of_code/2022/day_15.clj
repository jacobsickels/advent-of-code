(ns advent-of-code.2022.day-15
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [advent-of-code.shared.utils :as utils]
            [advent-of-code.shared.point :as point]))

(defn get-manhattan-points-around [[x1 y1] [x2 y2] height-filter]
  (let [distance (point/manhattan-distance [x1 y1] [x2 y2])
        top-point [x1 (+ y1 distance)]
        bottom-point [x1 (- y1 distance)]
        left-point [(- x1 distance) y1]
        right-point [(+ x1 distance) y1]
        min-x (ffirst (sort-by first [top-point bottom-point left-point right-point]))
        max-x (first (last (sort-by first [top-point bottom-point left-point right-point])))]
    (->> (utils/points-between [min-x height-filter] [max-x height-filter])
         (filter (fn [p1] (<= (point/manhattan-distance [x1 y1] p1) distance)))
         set)))

(def sensor-to-beacons (->> (read/read-file "resources/2022/day_15_example.txt")
                            (map #(re-seq #"-?\d+" %))
                            (map utils/parse-int-col)
                            (map #(partition 2 %))))

(def sensors (set (map first sensor-to-beacons)))
(def beacons (set (map second sensor-to-beacons)))

(defn get-points-with-sensors-and-beacons [height-filter]
  (loop [to-check sensor-to-beacons
         points #{}]
    (if (empty? to-check)
      points
      (let [[sensor beacon] (first to-check)
            points-around (get-manhattan-points-around sensor beacon height-filter)]
        (recur (rest to-check) (set/union points points-around))))))

(defn part-1 []
  (-> (get-points-with-sensors-and-beacons 2000000)         ; 2000000 10
      set
      (set/difference sensors)
      (set/difference beacons)
      count))

(defn get-beacon-ranges [[x1 y1] [x2 y2]]
  (println "TEST")
  (let [distance (point/manhattan-distance [x1 y1] [x2 y2])
        top-point [x1 (+ y1 distance)]
        bottom-point [x1 (- y1 distance)]]
    (->> (map
           #(list %1 %2)
           (utils/points-between top-point bottom-point)
           (concat (range 0 distance) (reverse (range 0 (inc distance)))))
         (reduce (fn [acc [[x y] amt]] (assoc acc y [[(- x amt) (+ x amt)]]))
                 {}))))


(defn should-merge?
  "What is returned is [to-merge, merged] we need to remove to-merge from ranges"
  [[r1a r1b] [r2a r2b]]
  (cond (<= r1a r2a r1b r2b) [[r2a r2b] [r1a r2b]]
        (<= r2a r1a r2b r1b) [[r2a r2b] [r2a r1b]]
        (<= r1a r2a r2b r1b) [[r2a r2b] [r1a r1b]]
        (<= r2a r1a r1b r2b) [[r2a r2b] [r2a r2b]]
        :else nil))

(defn merge-range [range ranges]
  (let [[to-merge merged] (->> (map (fn [r] (should-merge? range r)) (remove #(= range %) ranges))
                               (filter some?)
                               first)]
    (if (nil? merged)
      ranges
      (concat [merged] (remove #(or (= range %) (= to-merge %)) ranges)))))


(defn merge-whole-range [ranges]
  (loop [check-ranges ranges
         merged-ranges ranges
         merge-happened? false]
    (cond
      (and (empty? check-ranges) (not merge-happened?))
      merged-ranges
      (empty? check-ranges) (merge-whole-range merged-ranges)
      :else (let [new-merged (merge-range (first check-ranges) merged-ranges)]
              (recur (rest check-ranges)
                     new-merged
                     (not= (count new-merged) (count merged-ranges)))))))

(defn merge-to-complete [ranges]
  (let [merged (merge-whole-range ranges)]
    (if (= ranges merged)
      ranges
      (merge-to-complete merged))))

(defn merge-resolver [v1 v2]
  (merge-to-complete (concat v1 v2)))

(defn part-1-map []
  (let [ranges (get (->> sensor-to-beacons
                         (reduce (fn [acc [sensor beacon]] (merge-with merge-resolver acc (get-beacon-ranges sensor beacon))) {}))
                    2000000)]
    ranges))

;; After these were found I held onto them in my repl so I didn't have to recompute a bunch to save time
;(def range-map-test (->> (read/read-file "resources/2022/day_15_example.txt")
;                         (map #(re-seq #"-?\d+" %))
;                         (map parse-int-col)
;                         (map #(partition 2 %))
;                         (reduce (fn [acc [sensor beacon]] (merge-with merge-resolver acc (get-beacon-ranges sensor beacon))) {})))

;(def range-map-real (->> (read/read-file "resources/2022/day_15.txt")
;                         (map #(re-seq #"-?\d+" %))
;                         (map parse-int-col)
;                         (map #(partition 2 %))
;                         (reduce (fn [acc [sensor beacon]] (merge-with merge-resolver acc (get-beacon-ranges sensor beacon))) {})))

(defn has-single-gap? [ranges]
  (->> (sort-by first ranges)
       (flatten)
       (partition 2 1)
       (drop 1)
       (take-nth 2)
       (filter (fn [[a b]] (= 2 (- b a))))
       empty?
       not))

(defn get-single-gaps [range-map]
  (let [[y-val ranges] (->> (filter (fn [[k v]] (has-single-gap? v)) range-map)
                            (filter (fn [[y-val ranges]] (< 0 y-val 4000000)))
                            first)
        x-val (->> (sort-by first ranges)
                   (flatten)
                   (partition 2 1)
                   (drop 1)
                   (take-nth 2)
                   first
                   (apply range)
                   last)]
    (+ y-val (* x-val 4000000))))


