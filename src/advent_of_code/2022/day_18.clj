(ns advent-of-code.2022.day-18
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]))

(def test-data (->> (read/read-file "resources/2022/day_18_example.txt")
                    (map #(re-seq #"\d+" %))
                    (map utils/parse-int-col)
                    set))

(def real-data (->> (read/read-file "resources/2022/day_18.txt")
                    (map #(re-seq #"\d+" %))
                    (map utils/parse-int-col)
                    set))

(defn open-faces-count [[x y z] points]
  (->> (list [(inc x) y z]
             [(dec x) y z]
             [x (inc y) z]
             [x (dec y) z]
             [x y (inc z)]
             [x y (dec z)])
       (remove #(contains? points %))
       count))

(defn part-1 [data]
  (->> (map #(open-faces-count % data) data)
       (reduce +)))

(defn largest-smallest-points [data]
  (let [sorted-x (sort-by first data)
        sorted-y (sort-by second data)
        sorted-z (sort-by last data)
        lowest-x (first sorted-x)
        largest-x (last sorted-x)
        lowest-y (first sorted-y)
        largest-y (last sorted-y)
        lowest-z (first sorted-z)
        largest-z (last sorted-z)]
    [[(dec (first lowest-x))
      (dec (second lowest-y))
      (dec (last lowest-z))]
     [(inc (first largest-x))
      (inc (second largest-y))
      (inc (last largest-z))]]))

(def smallest-real (first (largest-smallest-points real-data)))
(def largest-real (second (largest-smallest-points real-data)))
(def smallest-test (first (largest-smallest-points test-data)))
(def largest-test (second (largest-smallest-points test-data)))

(defn is-outside-cube? [[x y z] smallest largest]
  (or (< x (first smallest))
      (> x (first largest))
      (< y (second smallest))
      (> y (second largest))
      (< z (last smallest))
      (> z (last largest))))

(defn open-faces [[x y z] points smallest largest]
  (->> (list [(inc x) y z]
             [(dec x) y z]
             [x (inc y) z]
             [x (dec y) z]
             [x y (inc z)]
             [x y (dec z)])
       (remove #(or (contains? points %) (is-outside-cube? % smallest largest)))))

(defn fill-box [points smallest largest]
  (loop [visited #{smallest}]
    (let [new-visited (set/union visited (set (mapcat #(open-faces % points smallest largest) visited)))]
      (if (= new-visited visited)
        visited
        (recur new-visited)))))

(defn open-faces-around-count [fill-box [x y z]]
  (->> (list [(inc x) y z]
             [(dec x) y z]
             [x (inc y) z]
             [x (dec y) z]
             [x y (inc z)]
             [x y (dec z)])
       (filter #(contains? fill-box %))
       count))

(defn part-2 [data smallest largest]
  (let [fill (fill-box data smallest largest)]
    (->> (map #(open-faces-around-count fill %) data)
         (reduce +))))

;; (part-2 test-data smallest-test largest-test)
;; => 58
;; (part-2 real-data smallest-real largest-real)
;; => 2008
