(ns advent-of-code.2022.day-04
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]))

(def data (->> (read/read-file "resources/2022/day_04.txt")
               (map #(re-seq #"\d+" %))
               (map utils/parse-int-col)
               (map #(partition 2 %))))

(defn overlapping? [[[s1x s1y] [s2x s2y]]]
  (let [f (range s1x (inc s1y))
        s (range s2x (inc s2y))]
    (or (set/subset? (set f) (set s))
        (set/subset? (set s) (set f)))))

(defn part-1 []
  (->> (map overlapping? data)
       (filter true?)
       count))

(defn overlapping-at-all? [[[s1x s1y] [s2x s2y]]]
  (let [f (range s1x (inc s1y))
        s (range s2x (inc s2y))]
    (set/intersection (set f) (set s))))

(defn part-2 []
  (->> (map overlapping-at-all? data)
       (remove empty?)
       count))


;; ============================================
;; In retrospect, sets are a bit of overkill
;; Below is a solution without using sets

(defn overlapping-no-set? [[[s1x s1y] [s2x s2y]]]
  (or (<= s1x s2x s2y s1y)
      (<= s2x s1x s1y s2y)))

(defn overlapping-at-all-no-set? [[[s1x s1y] [s2x s2y]]]
  (or (<= s1x s2x s1y)
      (<= s1x s2y s1y)
      (<= s2x s1x s2y)
      (<= s2x s1y s2y)))

(defn get-no-set-answer [fn]
  (->> (map fn data)
       (filter true?)
       count))

(defn part-1-no-set [] (get-no-set-answer overlapping-no-set?))
(defn part-2-no-set [] (get-no-set-answer overlapping-at-all-no-set?))