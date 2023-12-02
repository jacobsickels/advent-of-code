(ns advent-of-code.2023.day-02
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))


(defn parse-line [line]
  (let [[game values] (str/split line #": ")]
    {:id     (->> (str/split game #" ")
                  second
                  Integer/parseInt)
     :values (->> (str/split values #"; ")
                  (map #(->> (str/split % #", ")
                             (reduce (fn [acc l]
                                       (let [[num-cubes color] (str/split l #" ")]
                                         (assoc acc (keyword color) (Integer/parseInt num-cubes))))
                                     {}))))}))


(def data (->> (read/read-file "resources/2023/day_02.txt")
               (map parse-line)))

(def cubes {:red 12 :green 13 :blue 14})

(def test-game {:id     100,
                :values [{:green 5, :blue 11, :red 6}
                         {:green 5, :blue 12}
                         {:green 1, :blue 14, :red 1}
                         {:blue 3, :red 5, :green 6}
                         {:blue 9}
                         {:red 6}]})

(defn is-valid-game? [game]
  (->> (map #(and (>= (:red cubes) (get % :red 0))
                  (>= (:green cubes) (get % :green 0))
                  (>= (:blue cubes) (get % :blue 0)))
            (:values game))
       (filter false?)
       count
       zero?))

(defn part-1 []
  (reduce + (map #(if (is-valid-game? %) (:id %) 0) data)))

(defn get-power [game]
  (->> (reduce #(merge-with max %1 %2) {} (:values game))
       vals
       (apply *)))

(defn part-2 []
  (reduce + (map get-power data)))
