(ns advent-of-code.2025.day-07
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2025/day_07.txt")))

(def points (->> (map #(str/split % #"") data)
                 (map-indexed (fn [y line]
                                (map-indexed (fn [x space] [space x y]) line)))
                 (apply concat)))

(def height (count data))

(def splitters (->> (filter #(= "^" (first %)) points)
                    (map (fn [[p x y]] [x y]))
                    set))

(def start (->> (filter #(= "S" (first %)) points)
                (map (fn [[p x y]] [x y]))
                first))

(defn get-next-beams [beams]
  (let [next-beams (map #(let [[x y] %]
                           (if (contains? splitters [x (inc y)])
                             {:beams [[(dec x) (inc y)] [(inc x) (inc y)]] :split true}
                             {:beams [[x (inc y)]] :split false}))
                        beams)]
    [(set (mapcat :beams next-beams)) (get (frequencies (map :split next-beams)) true 0)]))

(defn part-1 []
  (loop [beams #{start}
         splits 0
         h 0]
    (if (= h height)
      [beams splits]
      (let [[next-beams s] (get-next-beams beams)]
        (recur next-beams (+ splits s) (inc h))))))

(defn get-next-beams-freq
  "Using a frequencies map to trim down a lot of extra beam data"
  [beam-freq]
  (->> (map (fn [[point freq]]
              (let [[x y] point]
                (if (contains? splitters [x (inc y)])
                  {[(dec x) (inc y)] freq
                   [(inc x) (inc y)] freq}
                  {[x (inc y)] freq})))
            beam-freq)
       (apply merge-with +)))

(defn part-2 []
  (loop [beams {start 1}
         h 0]
    (if (= h height)
      (reduce + (vals beams))
      (let [next-beams (get-next-beams-freq beams)]
        (recur next-beams  (inc h))))))