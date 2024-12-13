(ns advent-of-code.2024.day-13
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.string :as str]))

(defn parse-nums [col]
  (-> (str/split col #": ")
      second
      (str/split #", ")
      (->> (map #(re-seq #"\d+" %))
           flatten
           utils/parse-int-col)))

(defn parse-machine [col]
  (let [[x1 y1] (parse-nums (first col))
        [x2 y2] (parse-nums (second col))]
    {:a [x1 x2]
     :b [y1 y2]
     :prize (parse-nums (last col))}))

(def data (->> (read/read-file "resources/2024/day_13.txt")
               (partition-by empty?)
               (filter #(= 3 (count %)))
               (map parse-machine)))

(defn solve-equations [machine]
  (let [[ax ay] (:a machine)
        [bx by] (:b machine)
        [px py] (:prize machine)
        val-a (/
                (- (* px by) (* py ay))
                (- (* ax by) (* bx ay)))
        val-b (/ (- px (* ax val-a)) ay)]
    {:a val-a :b val-b}))

(defn is-machine-possible? [machine]
  (let [solution (solve-equations machine)]
    (and (not (ratio? (:a solution)))
         (not (ratio? (:b solution))))))

(defn cost [solution]
  (+ (* 3 (:a solution)) (:b solution)))

(defn part-1 []
  (->> (filter is-machine-possible? data)
       (map solve-equations)
       (map cost)
       (reduce +)))

(defn part-2 []
  (->> (map #(assoc % :prize (map (fn [p] (+ p 10000000000000)) (:prize %))) data)
       (filter is-machine-possible?)
       (map solve-equations)
       (map cost)
       (reduce +)))