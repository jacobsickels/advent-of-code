(ns advent-of-code.2018.day-08
  (:require [advent-of-code.shared.read-file :as read]))

(def test-data [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
(def test-data-2 [2 3 1 1 0 1 99 2 0 3 10 11 12 1 1 2])

(defn partition-keep-remainder [n col]
  (if (zero? n)
    col
    (let [partition-number (quot (count col) n)
          elements (partition-all (quot (count col) n) col)]
      (if (or (not= (count elements) n) (not= partition-number (count (last elements))))
        (conj (drop-last 2 elements) (apply concat (take-last 2 elements)))
        elements))))

(defn part-1 [col])

(defn- sum-meta [col pointer meta-length]
  (reduce + (take meta-length (drop pointer col))))

(defn parse-children [col]
  (loop [pointer 0
         meta 0]
    (cond

      (zero? (get col pointer))
      (let [inc-pointer (get col (inc pointer))
            inc-meta (sum-meta col pointer (get col (inc pointer)))]
        (recur
          (+ pointer inc-pointer)
          (+ meta inc-meta))))))


(defn- format-children [col sum-meta]
  (let [[num-children num-metadata & rest] col
        children (drop-last num-metadata rest)]
    (format-children rest (reduce + (take-last num-metadata rest)))))


(defn make-structure [col]
  (let [_ (println "TAKE" col (take 2 col))
        [num-children num-metadata] (take 2 col)
        children (drop-last num-metadata (drop 2 col))
        found-children (when-not (zero? num-children)
                         (partition-keep-remainder num-children children))
        _ (println "CHILDREN"  children num-children found-children)]
    {:meta-data (take-last num-metadata col)
     :col children
     :children  (map make-structure found-children)}))
;
;
;;; 2 3 1 1 0 1 99 2 0 3 10 11 12 1 1 2
;(defn make-all-meta [col]
;  (let [[num-children num-metadata] (take 2 col)
;        children-col (drop-last num-metadata (drop 2 col))
;        found-children (partition-keep-remainder num-children children-col)
;        _ (println num-children children-col found-children)]
;    (+
;      (reduce + (map make-all-meta found-children))
;      (reduce + (take-last num-metadata col)))))
;
;(defn part-1 []
;  (let [data (read/load-edn "2018/day_08.edn")
;        meta-data-sum (make-all-meta data)]
;    meta-data-sum))