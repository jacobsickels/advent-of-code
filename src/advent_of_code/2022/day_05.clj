(ns advent-of-code.2022.day-05
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]))

(def crates ['(\H \T \Z \D)
             '(\Q \R \W \T \G \C \S)
             '(\P \B \F \Q \N \R \C \H)
             '(\L \C \N \F \H \Z)
             '(\G \L \F \Q \S)
             '(\V \P \W \Z \B \R \C \S)
             '(\Z \F \J)
             '(\D \L \V \Z \R \H \Q)
             '(\B \H \G \N \F \Z \L \D)])

(def moves (->> (read/read-file "resources/2022/day_05.txt")
                (map #(re-seq #"\d+" %))
                (map utils/parse-int-col)))

(defn move-crate [crates from to]
  (let [to-move (last (nth crates (dec from)))]
    (-> (update crates (dec to) concat [to-move])
        (update (dec from) butlast))))

(defn move-crate-times [crates times from to]
  (loop [t 0
         acc crates]
    (if (= t times)
      acc
      (recur (inc t) (move-crate acc from to)))))

(defn part-1 []
  (loop [m moves
         c crates]
    (if (empty? m)
      c
      (let [[times from to] (first m)]
        (recur (rest m) (move-crate-times c times from to))))))

(defn move-many-crates [crates amount from to]
  (let [to-move (take-last amount (nth crates (dec from)))]
    (-> (update crates (dec to) concat to-move)
        (update (dec from) (fn [l] (drop-last amount l))))))

(defn part-2 []
  (loop [m moves
         c crates]
    (if (empty? m)
      c
      (let [[amount from to] (first m)]
        (recur (rest m) (move-many-crates c amount from to))))))