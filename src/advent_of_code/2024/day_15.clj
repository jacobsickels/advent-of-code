(ns advent-of-code.2024.day-15
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_15.txt")
               (partition-by empty?)))


(def warehouse (first data))
(def movements (->> (last data) (apply str)))

(defn find-symbols [s]
  (->> (map-indexed (fn [i line]
                      (map-indexed
                        (fn [j space] (list [j i] space))
                        (str/split line #""))) warehouse)
       (mapcat (fn [col] (filter #(= s (second %)) col)))
       (map first)))

(def walls (set (find-symbols "#")))
(def boxes (set (find-symbols "O")))
(def robot (first (find-symbols "@")))

(defn cast-movement-col [robot boxes movement]
  (loop [pos robot
         acc []]
    (let [check-pos (mapv #(%1 %2) movement pos)]
      (if (contains? walls check-pos)
        acc
        (cond
          (contains? boxes check-pos)
          (recur check-pos (conj acc (conj check-pos "O")))

          :else
          (recur check-pos (conj acc (conj check-pos "."))))))))

(defn do-movement [robot boxes movement]
  (let [casted-movement (cast-movement-col robot boxes movement)
        contains-freespace? (contains? (set (map last casted-movement)) ".")]
    (if contains-freespace?
      (let [moved-robot (mapv #(%1 %2) movement robot)
            first-freespace (first (filter #(= "." (last %)) casted-movement))
            moved-boxes (->> (partition-by #(= first-freespace %) casted-movement)
                             first
                             (filter (fn [[_ _ b]] (= "O" b)))
                             (map (fn [[x y _]] [x y])))]
        [moved-robot (set/union (set/difference boxes moved-boxes)
                                (->> (map (fn [box] (mapv #(%1 %2) movement box)) moved-boxes)
                                     set))])
      [robot boxes])))

(defn translate-movement [ch]
  (get {\< [dec identity]
        \v [identity inc]
        \> [inc identity]
        \^ [identity dec]} ch))

(defn do-all-movements []
  (loop [moves movements
         r robot
         bxs boxes]
    (if (empty? moves)
      bxs
      (let [[new-robot new-boxes] (do-movement r bxs (translate-movement (first moves)))]
        (recur (rest moves) new-robot new-boxes)))))

(defn part-1 []
  (->> (do-all-movements)
       (map (fn [[x y]] (+ x (* 100 y))))
       (reduce +)))

;; ====================================================

(def walls-2 (set (map (fn [[x y]] [[x (+ x 0.5)] y]) walls)))
(def boxes-2 (set (map (fn [[x y]] [[x (+ x 0.5)] y]) boxes)))

(defn get-overlapping [check-box boxes]
  (let [[[x1 x2] y] check-box]
    (->> (filter (fn [[bx-r by]] (= y by)) boxes)
         (filter (fn [[[bx1 bx2] _]] (or (<= bx1 x1 bx2) (<= bx1 x2 bx2)))))))

(defn translate-movement-robot [ch]
  (get {\< [(fn [x] (- x 0.5)) identity]
        \v [identity inc]
        \> [(fn [x] (+ x 0.5)) identity]
        \^ [identity dec]} ch))

(defn translate-checked [check movement-char]
  (map #(%2 %1) check (get {\< [(fn [[x1 x2]] [(- x1 0.5) (- x2 0.5)]) identity]
                            \v [identity inc]
                            \> [(fn [[x1 x2]] [(+ x1 0.5) (+ x2 0.5)]) identity]
                            \^ [identity dec]} movement-char)))

(defn is-robot-in-col [robot boxes]
  (let [[x y] robot]
    (filter (fn [[[bx1 bx2] by]] (and (<= bx1 x bx2) (= y by))) boxes)))

(defn cast-ray
  "Take the movement vector and find all the spaces and boxes in a line.
   Stops when encounters a wall. If a wall is encountered this will contain a nil element."
  [check boxes walls movement-char]
  (let [translated-check (translate-checked check movement-char)
        in-boxes (get-overlapping translated-check (set/difference boxes (set [check])))
        in-walls (get-overlapping translated-check walls)]
    (cond
      (not-empty in-walls) [nil]
      (empty? in-boxes) [check]

      :else
      (let [next-checks (mapcat #(cast-ray % boxes walls movement-char) in-boxes)]
        (conj next-checks check)))))

(defn move-robot [robot boxes walls movement-char]
  (let [moved-robot (mapv #(%1 %2) (translate-movement-robot movement-char) robot)
        robot-overlapping-box (first (is-robot-in-col moved-robot boxes))
        robot-overlapping-wall (first (is-robot-in-col moved-robot walls))]
    (cond
      (not (nil? robot-overlapping-wall))
      [robot boxes] ;; robot is moving into a wall

      (nil? robot-overlapping-box)
      [moved-robot boxes] ;; space was empty for robot to move to

      :else
      (let [boxes-to-move (cast-ray robot-overlapping-box boxes walls movement-char)]
        (if (some nil? boxes-to-move)
          [robot boxes] ;; there was a wall above a box so this can't be pushed
          [moved-robot
           (set/union (set/difference boxes boxes-to-move)
                      (->> (map #(translate-checked % movement-char) boxes-to-move)
                           set))])))))

(defn do-movements-2 []
  (loop [moves movements
         r robot
         bxs boxes-2]
    (if (empty? moves)
      bxs
      (let [[new-robot new-boxes] (move-robot r bxs walls-2 (first moves))]
        (recur (rest moves) new-robot new-boxes)))))

(defn part-2 []
  (->> (do-movements-2)
       (map (fn [[[x1 x2] y]] (+ (* 2 x1) (* 100 y))))
       (reduce +)
       int))