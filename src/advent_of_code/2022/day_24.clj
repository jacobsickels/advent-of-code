(ns advent-of-code.2022.day-24
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [advent-of-code.shared.point :as points]
            [clojure.set :as set]))

(def data (->> (read/read-file "resources/2022/day_24_example.txt")
               (map-indexed (fn [i line]
                              (map-indexed
                                (fn [j space] (list [j i] space))
                                (str/split line #""))))
               (apply concat)
               set))

(def walls (->> (filter #(= "#" (second %)) data)
                (map first)
                set
                (set/union #{[1 -1] [6 6]})))               ;; Change these for real data

(def tornadoes (->> (remove #(or (= "." (second %)) (= "#" (second %))) data)))

(defn next-tornado-position [tornado min-x max-x min-y max-y]
  (let [[[x y] direction] tornado]
    [(cond (= direction ">")
           (if (< max-x (inc x))
             [min-x y]
             [(inc x) y])

           (= direction "<")
           (if (> min-x (dec x))
             [max-x y]
             [(dec x) y])

           (= direction "^")
           (if (> min-y (dec y))
             [x max-y]
             [x (dec y)])

           :else (if (< max-y (inc y))
                   [x min-y]
                   [x (inc y)])) direction]))

(defn move-tornadoes [tornadoes min-x max-x min-y max-y]
  (map #(next-tornado-position % min-x max-x min-y max-y) tornadoes))

(defn next-moves [position walls tornadoes start-position]
  (let [around (points/cardinal-points-around position)]
    (-> (set/difference (set around) #{start-position})
        (set/difference walls)
        (set/difference (set (map first tornadoes))))))

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
    ;(println positions steps)
    (cond
      (contains? positions end-position)
      [steps t]

      :else (let [_ "spacing"
                  [min-x max-x] [1 6]
                  [min-y max-y] [1 4]
                  ;[min-x max-x] [1 120]
                  ;[min-y max-y] [1 25]
                  next-tornadoes (move-tornadoes t min-x max-x min-y max-y)
                  new-positions (next-positions positions walls next-tornadoes start-position)]
              (recur next-tornadoes
                     new-positions
                     (inc steps))))))

(defn walk-across [start-pos end-pos walls tornadoes]
  (let [[first-across first-tornadoes] (move-to-end start-pos end-pos walls tornadoes)
        _ (println "done first")
        [second-across second-tornadoes] (move-to-end end-pos start-pos walls first-tornadoes)
        _ (println "done second")
        [third-across _] (move-to-end start-pos end-pos walls second-tornadoes)
        _ (println "done third")]
    [first-across second-across third-across]))


