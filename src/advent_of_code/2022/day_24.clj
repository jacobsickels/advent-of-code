(ns advent-of-code.2022.day-24
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [advent-of-code.shared.point :as points]
            [clojure.set :as set]))

(def data (->> (read/read-file "resources/2022/day_24.txt")
               (map-indexed (fn [i line]
                              (map-indexed
                                (fn [j space] (list [j i] space))
                                (str/split line #""))))
               (apply concat)
               set))

(def walls (->> (filter #(= "#" (second %)) data)
                (map first)
                set
                (set/union #{[1 -1] [6 6]}))) ;; Change these for real data

(def tornadoes (->> (remove #(or (= "." (second %)) (= "#" (second %))) data)))

(defn next-tornado-position [tornado area-width area-height]
  (let [[[x y] direction] tornado]
    [(cond (= direction ">")
           (if (<  area-width (inc x))
             [1 y]
             [(inc x) y])

           (= direction "<")
           (if (= 0 (dec x))
             [area-width y]
             [(dec x) y])

           (= direction "^")
           (if (= 0 (dec y))
             [x  area-height]
             [x (dec y)])

           :else (if (< area-height (inc y))
                   [x 1]
                   [x (inc y)])) direction]))

(defn move-tornadoes [tornadoes area-width area-height]
  (map #(next-tornado-position % area-width area-height) tornadoes))

(defn next-moves [position walls tornadoes]
  (println "position" position)
  (let [around (points/cardinal-points-around position)]
    (-> (set/difference (set around) walls)
        (set/difference (set (map first tornadoes))))))

(defn next-positions [positions walls tornadoes]
  (set (mapcat #(next-moves % walls tornadoes) positions)))

(defn should-stop? [lines end-position]
  (or
    (->> (map count lines)
         (filter #(> % 18))
         (empty?)
         not)
    (->> (map last lines)
         (filter #(= end-position %))
         (empty?)
         not)))

(defn move-to-end [start-position end-position walls tornadoes]
  (loop [t tornadoes
         positions #{start-position}
         steps 0]
    (if (contains? positions end-position)
      steps
      (let [next-tornadoes (move-tornadoes t 6 4)
            new-positions (next-positions positions walls next-tornadoes)]
        (if (empty? new-positions)
          (recur next-tornadoes
                 positions
                 (inc steps))
          (recur next-tornadoes
                 (next-positions positions walls next-tornadoes)
                 (inc steps)))))))


