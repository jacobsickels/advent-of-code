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
      [r bxs]
      (let [[new-robot new-boxes] (do-movement r bxs (translate-movement (first moves)))]
        (recur (rest moves) new-robot new-boxes)))))

(defn part-1 []
  (let [[r b] (do-all-movements)]
    (->> (map (fn [[x y]] (+ x (* 100 y))) b)
         (reduce +))))

;; ====================================================

(def walls-2 (set (map (fn [[x y]] [[x (+ x 0.5)] y]) walls)))
(def boxes-2 (set (map (fn [[x y]] [[x (+ x 0.5)] y]) boxes)))

(defn get-overlapping [check-box boxes]
  (let [[[x1 x2] y] check-box]
    (->> (filter (fn [[bx-r by]] (= y by)) boxes)
         (filter (fn [[[bx1 bx2] _]] (or (<= bx1 x1 bx2) (<= bx1 x2 bx2)))))))

(defn translate-movement-2 [ch]
  (get {\< [(fn [[x1 x2]] [(- x1 0.5) (- x2 0.5)]) identity]
        \v [identity inc]
        \> [(fn [[x1 x2]] [(+ x1 0.5) (+ x2 0.5)]) identity]
        \^ [identity dec]} ch))

(defn translate-movement-robot [ch]
  (get {\< [(fn [x] (- x 0.5)) identity]
        \v [identity inc]
        \> [(fn [x] (+ x 0.5)) identity]
        \^ [identity dec]} ch))

(defn translate-checked [check movement-char]
  (map #(%2 %1) check (translate-movement-2 movement-char)))

(defn cast-vertical-movement [check boxes walls movement-char]
  (let [translated-check (translate-checked check movement-char)
        in-boxes (get-overlapping translated-check boxes)
        in-walls (get-overlapping translated-check walls)]
    (cond
      (not-empty in-walls) [nil]
      (empty? in-boxes) [check]

      :else
      (let [next-checks (mapcat #(cast-vertical-movement % boxes walls movement-char) in-boxes)]
        (conj next-checks check)))))

(defn is-robot-in-col [robot boxes]
  (let [[x y] robot]
    (filter (fn [[[bx1 bx2] by]] (and (<= bx1 x bx2) (= y by))) boxes)))

(defn do-vertical-movement [robot boxes walls movement-char]
  (let [moved-robot (mapv #(%1 %2) (translate-movement-robot movement-char) robot)
        robot-overlapping-box (first (is-robot-in-col moved-robot boxes))
        robot-overlapping-wall (first (is-robot-in-col moved-robot walls))]
    (cond
      (not (nil? robot-overlapping-wall))
      [robot boxes] ;; robot is moving into a wall

      (nil? robot-overlapping-box)
      [moved-robot boxes] ;; space was empty for robot to move to

      :else
      (let [boxes-to-move (cast-vertical-movement robot-overlapping-box boxes walls movement-char)]
        (if (some nil? boxes-to-move)
          [robot boxes] ;; there was a wall above a box so this can't be pushed
          [moved-robot
           (set/union (set/difference boxes boxes-to-move)
                      (->> (map #(translate-checked % movement-char) boxes-to-move)
                           set))])))))

(defn cast-horizontal-movement [check boxes walls movement-char]
  (let [translated-check (translate-checked check movement-char)
        in-boxes (get-overlapping translated-check (set/difference boxes (set [check])))
        in-walls (get-overlapping translated-check walls)]
    (println translated-check in-boxes in-walls)
    (cond
      (not-empty in-walls) [nil]
      (empty? in-boxes) [check]

      :else
      (let [next-checks (mapcat #(cast-horizontal-movement % boxes walls movement-char) in-boxes)]
        (conj next-checks check)))))

(defn do-horizontal-movement [robot boxes walls movement-char]
  (let [moved-robot (mapv #(%1 %2) (translate-movement-robot movement-char) robot)
        robot-overlapping-box (first (is-robot-in-col moved-robot boxes))
        robot-overlapping-wall (first (is-robot-in-col moved-robot walls))]
    (cond
      (not (nil? robot-overlapping-wall))
      [robot boxes] ;; robot is moving into a wall

      (nil? robot-overlapping-box)
      [moved-robot boxes] ;; space was empty for robot to move to

      :else
      (let [boxes-to-move (cast-horizontal-movement robot-overlapping-box boxes walls movement-char)]
        (if (some nil? boxes-to-move)
          [robot boxes] ;; there was a wall above a box so this can't be pushed
          [moved-robot
           (set/union (set/difference boxes boxes-to-move)
                      (->> (map #(translate-checked % movement-char) boxes-to-move)
                           set))])))))

(defn do-movement-2 [robot boxes walls movement-char]
  (if (contains? #{\< \>} movement-char)
    (do-horizontal-movement robot boxes walls movement-char)
    (do-vertical-movement robot boxes walls movement-char)))

(defn do-movements-2 []
  (loop [moves movements
         r robot
         bxs boxes-2]
    (if (empty? moves)
      [r bxs]
      (let [[new-robot new-boxes] (do-movement-2 r bxs walls-2 (first moves))]
        (recur (rest moves) new-robot new-boxes)))))

(defn part-2 []
  (let [[robot boxes] (do-movements-2)]
    (->> (map (fn [[[x1 x2] y]] (+ (* 2 x1) (* 100 y))) boxes)
         (reduce +))))
















;
;(defn- position-in-stretched-col [col [x y]]
;  (->> (filter (fn [[bx by]] (and (<= bx x (+ bx 0.5)) (= by y))) col)
;       (remove empty?)))
;
;(defn positions-in-stretched-col [col positions]
;  (->> (mapv #(position-in-stretched-col col %) positions)
;       (remove empty?)))
;
;(defn apply-movement-to-points [points movement]
;  (mapv (fn [point] (map #(%2 %1) point movement)) points))
;
;(defn translate-movement-2 [ch]
;  (get {\< [(fn [n] (- n 0.5)) identity]
;        \v [identity inc]
;        \> [(fn [n] (+ n 0.5)) identity]
;        \^ [identity dec]} ch))
;
;(defn cast-movement-horizontal [robot boxes movement-char]
;  (loop [acc [(conj robot "@")]]
;    (let [next-point (mapv #(%1 %2) (translate-movement movement-char) (last acc))
;          in-wall? (not-empty (position-in-stretched-col walls next-point))
;          free-space? (= "." (last (last acc)))]
;      (if (or in-wall? free-space?)
;        acc
;        (let [in-box? (not-empty (position-in-stretched-col boxes next-point))]
;          (if in-box?
;            (recur (conj acc (conj next-point "O")))
;            (recur (conj acc (conj next-point ".")))))))))
;
;(defn do-horizontal-movement [robot boxes movement-char]
;  (let [casted-movement (cast-movement-horizontal robot boxes movement-char)
;        contains-freespace? (= "." (last (last casted-movement)))]
;    (println casted-movement)
;    (if contains-freespace?
;      (let [moved-robot (mapv #(%1 %2) (translate-movement-2 movement-char) robot)
;            moved-boxes (->> (butlast (drop 1 casted-movement))
;                             (map (fn [[x y _]] [x y])))]
;        (println moved-boxes)
;        [moved-robot (set/union (set/difference boxes moved-boxes)
;                                (->> (map (fn [box] (mapv #(%1 %2) (translate-movement-2 movement-char) box)) moved-boxes)
;                                     set))])
;      [robot boxes])))
;
;(defn- box-in-stretched-col [col [x y]]
;  (->> (filter (fn [[bx by]] (or (and (<= bx x (+ bx 0.5)) (= by y))
;                                 (and (<= bx (+ x 0.5) (+ bx 0.5)) (= by y)))) col)
;       (remove empty?)))
;
;(defn get-moved-boxes-vertical [checked-point boxes movement-char]
;  (println checked-point)
;  (let [in-boxes (position-in-stretched-col boxes checked-point)]
;    (println "in-boxes" in-boxes)
;    (cond
;      (empty? in-boxes)
;      []
;
;      :else
;      (let [next-check-points (apply-movement-to-points in-boxes (translate-movement movement-char))]
;        (println "next" next-check-points)
;        (concat in-boxes (mapcat #(get-moved-boxes-vertical % boxes movement-char) next-check-points))))))
