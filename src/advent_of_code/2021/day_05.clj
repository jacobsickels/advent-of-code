(ns advent-of-code.2021.day-05
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.string :as str]))

;(def data
;  (map
;    (fn [[p1 p2]] (vec [(str/split p1 #",") (str/split p2 #",")]))
;    (map #(str/split % #" -> ") (read/read-file "resources/day_05.txt"))))

(def test-data (map
                 (fn [[p1 p2]] (vec [(str/split p1 #",") (str/split p2 #",")]))
                 (map #(str/split % #" -> ") ["0,9 -> 5,9"
                                              "8,0 -> 0,8"
                                              "9,4 -> 3,4"
                                              "2,2 -> 2,1"
                                              "7,0 -> 7,4"
                                              "6,4 -> 2,0"
                                              "0,9 -> 2,9"
                                              "3,4 -> 1,4"
                                              "0,0 -> 8,8"
                                              "5,5 -> 8,2"])))

(defn format-data [input]
  (map #(map (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)]) %) input))

(defn get-diagonal-points [[x1 y1] [x2 y2]]
  (let [x-range (apply utils/inclusive-range (sort [x1 x2]))
        x-range (if (= (first x-range) x1) x-range (reverse x-range))
        y-range (apply utils/inclusive-range (sort [y1 y2]))
        y-range (if (= (first y-range) y1) y-range (reverse y-range))]
    (map vec (apply zipmap [x-range y-range]))))

(defn points-between [[x1 y1] [x2 y2] diagonal-fn]
  (cond
    (= x1 x2)
    (map #(vec [x1 %]) (apply utils/inclusive-range (sort [y1 y2])))

    (= y1 y2)
    (map #(vec [% y1]) (apply utils/inclusive-range (sort [x1 x2])))

    :else
    (diagonal-fn [x1 y1] [x2 y2])))

(defn count-overlapping [input diagonal-fn]
  (->> (format-data input)
       (map (fn [[p1 p2]] (points-between p1 p2 diagonal-fn)))
       (apply concat)
       (frequencies)
       (filter #(>= (second %) 2))
       (count)))

(defn part-1 [input] (count-overlapping input (constantly nil)))
(defn part-2 [input] (count-overlapping input get-diagonal-points))
