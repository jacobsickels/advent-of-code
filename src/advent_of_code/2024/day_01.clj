(ns advent-of-code.2024.day-01
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_01.txt")
               (map #(str/split % #"   "))
               (map (fn [[x y]] (vector (Integer/parseInt x) (Integer/parseInt y))))))

(defn part-1 []
  (let [f (sort (map first data))
        s (sort (map second data))]
    (->> (map #(Math/abs (- %1 %2)) f s)
         (reduce +))))

(defn part-2 []
  (let [f (sort (map first data))
        freq (frequencies (map second data))]
    (->> (map #(* % (get freq % 0)) f)
         (reduce +))))