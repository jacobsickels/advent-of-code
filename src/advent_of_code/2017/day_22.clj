(ns advent-of-code.2017.day-22
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2017/day_22.txt")
               (partition-by empty?)))

(def width (count (ffirst data)))
(def height (count (first data)))

(defn find-symbols [s]
  (->> (map-indexed (fn [i line]
                      (map-indexed
                        (fn [j space] (list [j i] space))
                        (str/split line #""))) (first data))
       (mapcat (fn [col] (filter #(= s (second %)) col)))
       (map first)))

(def starting-infections (set (find-symbols "#")))
(def start [(quot width 2) (quot height 2)])

(defn turn-right [direction]
  (get {:up :right :right :down :down :left :left :up} direction))

(defn turn-left [direction]
  (get {:up :left :right :up :down :right :left :down} direction))

(defn turn-around [direction]
  (get {:up :down :right :left :down :up :left :right} direction))

(defn move-forward [position direction]
  (let [[x y] position]
    (cond
      (= :up direction)
      [x (dec y)]

      (= :right direction)
      [(inc x) y]

      (= :down direction)
      [x (inc y)]

      (= :left direction)
      [(dec x) y])))

(defn part-1 []
  (loop [carrier-pos start
         carrier-dir :up
         infections starting-infections
         bursts-caused-infection 0
         moves 0]
    (if (= moves 10000)
      [infections (count infections) carrier-pos carrier-dir bursts-caused-infection]
      (let [next-direction (if (contains? infections carrier-pos) (turn-right carrier-dir) (turn-left carrier-dir))
            [next-infections infected?] (if (contains? infections carrier-pos) [(set/difference infections #{carrier-pos}) false] [(set/union infections #{carrier-pos}) true])]
        (recur (move-forward carrier-pos next-direction)
               next-direction
               next-infections
               (if infected? (inc bursts-caused-infection) bursts-caused-infection)
               (inc moves))))))


(defn decide-turn [position direction weakened infected flagged]
  (cond (contains? weakened position)
        direction

        (contains? infected position)
        (turn-right direction)

        (contains? flagged position)
        (turn-around direction)

        :else
        (turn-left direction)))

(defn next-infections-state [position infections weakened flagged]
  "false here is for if it was added to infections"
  (cond (contains? weakened position)
        [(set/union infections #{position}) (set/difference weakened #{position}) flagged true]

        (contains? infections position)
        [(set/difference infections #{position})  weakened (set/union flagged #{position}) false]

        (contains? flagged position)
        [infections weakened (set/difference flagged #{position}) false]

        :else
        [infections (set/union weakened #{position}) flagged false]))

(defn part-2 []
  (loop [carrier-pos start
         carrier-dir :up
         infections starting-infections
         weakened #{}
         flagged #{}
         bursts-caused-infection 0
         moves 0]
    (if (= moves 10000000)
      [infections (count infections) carrier-pos carrier-dir bursts-caused-infection]
      (let [next-direction (decide-turn carrier-pos carrier-dir weakened infections flagged)
            [next-infections next-weakened next-flagged infected?] (next-infections-state carrier-pos infections weakened flagged)]
        (recur (move-forward carrier-pos next-direction)
               next-direction
               next-infections
               next-weakened
               next-flagged
               (if infected? (inc bursts-caused-infection) bursts-caused-infection)
               (inc moves))))))