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
                set))               ;; Change these for real data

(def tornadoes (->> (remove #(or (= "." (second %)) (= "#" (second %))) data)))

(defn next-tornado-position [tornado walls]
  (let [x-sort (sort-by first walls)
        y-sort (sort-by second walls)
        min-x (first (first x-sort))
        max-x (first (last x-sort))
        min-y (last (first y-sort))
        max-y (last (last y-sort))
        [[x y] direction] tornado]
    [(cond (= direction ">")
           (if (= (dec max-x) x)
             [(inc min-x) y]
             [(inc x) y])

           (= direction "<")
           (if (= (inc min-x) x)
             [(dec max-x) y]
             [(dec x) y])

           (= direction "^")
           (if (= (inc min-y) y)
             [x (dec max-y)]
             [x (dec y)])

           :else (if (= (dec max-y) y)
                   [x (inc min-y)]
                   [x (inc y)])) direction]))

(defn move-tornadoes [tornadoes]
  (map #(next-tornado-position % walls) tornadoes))

(defn next-moves [position walls tornadoes start-position]
  (let [x-sort (sort-by first walls)
        y-sort (sort-by second walls)
        max-x (first (last x-sort))
        max-y (last (last y-sort))
        around (->> (points/cardinal-points-around position)
                    (filter (fn [[x y]] (and (>= x 0) (>= y 0)
                                             (<= x max-x) (<= y max-y)))))]
    (-> (set/difference (set around) #{start-position})
        (set/difference walls)
        (set/difference (set/difference (set (map first tornadoes)))))))

(defn next-positions [positions walls tornadoes start-position]
  (set (mapcat #(let [n-moves (next-moves % walls tornadoes start-position)]
                  (if (empty? n-moves)
                    #{%}
                    n-moves))
               positions)))

(defn move-to-end [start-position end-position walls tornadoes]
  (loop [t tornadoes
         positions #{start-position}
         steps 0]
    (println steps)
    (cond
      (contains? positions end-position)
      [steps t]

      :else (let [next-tornadoes (move-tornadoes t)
                  new-positions (next-positions positions
                                                walls
                                                next-tornadoes
                                                start-position)]
              (recur next-tornadoes
                     new-positions
                     (inc steps))))))

(defn walk-across [start-pos end-pos walls tornadoes]
  (let [[first-across first-tornadoes _] (move-to-end start-pos end-pos walls tornadoes)
        _ (println "done first")
        [second-across second-tornadoes] (move-to-end end-pos start-pos walls first-tornadoes)
        _ (println "done second")
        [third-across _] (move-to-end start-pos end-pos walls second-tornadoes)
        _ (println "done third")]
    [first-across second-across third-across]))


