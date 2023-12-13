(ns advent-of-code.2023.day-13
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]))

(def data (->> (read/read-file "resources/2023/day_13.txt")
               (partition-by empty?)
               (remove #(empty? (first %)))))

(defn rotate-board [board]
  (->> (map (fn [n] (map #(nth % n) board))
            (range 0 (count (first board))))
       (map #(apply str %))))

(defn find-reflection [board starting-column]
  (loop [columns [starting-column (inc starting-column)]]
    (cond
      (and (nil? (nth board (first columns) nil)) (not (nil? (nth board (second columns) nil))))
      [starting-column (inc starting-column)]

      (and (nil? (nth board (second columns) nil)) (not (nil? (nth board (first columns) nil))))
      [starting-column (inc starting-column)]

      (= (nth board (first columns) nil) (nth board (second columns) nil))
      (recur [(dec (first columns)) (inc (second columns))])

      :else nil)))

(defn find-reflected-column [board]
  (let [rotated (rotate-board board)
        reflection-columns (->> rotated
                                (map-indexed (fn [i g] [i g]))
                                (partition-by second)
                                (filter #(= 2 (count %)))
                                (map ffirst))]
    (if (empty? reflection-columns)
      nil
      (->> (map #(find-reflection rotated %) reflection-columns)
           (remove nil?)
           first))))

(defn find-reflected-row [board]
  (let [rotated board
        reflection-columns (->> rotated
                                (map-indexed (fn [i g] [i g]))
                                (partition-by second)
                                (filter #(= 2 (count %)))
                                (map ffirst))]
    (if (empty? reflection-columns)
      nil
      (->> (map #(find-reflection rotated %) reflection-columns)
           (remove nil?)
           first))))

(defn part-1 []
  (->> (map #(array-map :horizontal (find-reflected-row %)
                        :vertical (find-reflected-column %)) data)
       (reduce (fn [acc m]
                 (cond


                   (nil? (:vertical m))
                   (+ acc (* 100 (inc (first (:horizontal m)))))

                   :else
                   (+ acc (inc (first (:vertical m))))))
               0)))

;; ===============================================

(defn how-many-row-off [row1 row2]
  (->> (map = row1 row2)
       (filter false?)
       count))

(defn is-row-off-by-one? [row1 row2]
  (->> (how-many-row-off row1 row2)
       (= 1)))

(defn get-rows-off-by-one [row board]
  (->> (map-indexed (fn [i b] (vector (is-row-off-by-one? row b) row b i)) board)))

(defn find-row-off-by-one [board]
  (->> (mapcat #(get-rows-off-by-one % board) board)
       (filter #(true? (first %)))))

(defn replace-smudge-in-row [board]
  (let [smudge-rows (find-row-off-by-one board)]
    (if (empty? smudge-rows)
      nil
      (map
        (fn [smudge-row]
          (let [[_ smudge-row replace-row smudge-index] smudge-row]
            (assoc (vec board) smudge-index smudge-row)))
        smudge-rows))))

(defn get-reflection-row [board]
  (->> board
       (map-indexed (fn [i g] [i g]))
       (partition-by second)
       (filter #(= 2 (count %)))
       (map ffirst)))

(defn is-valid-reflection? [board column]
  (loop [columns [column (inc column)]]
    (cond
      (and (nil? (nth board (first columns) nil)) (not (nil? (nth board (second columns) nil))))
      true

      (and (nil? (nth board (second columns) nil)) (not (nil? (nth board (first columns) nil))))
      true

      (= (nth board (first columns) nil) (nth board (second columns) nil))
      (recur [(dec (first columns)) (inc (second columns))])

      :else false)))

(defn find-valid-reflection [board replaced-smudge-board]
  (let [old-reflection-columns (filter #(is-valid-reflection? board %) (get-reflection-row board))]
    (if (nil? replaced-smudge-board)
      nil
      (let [reflection-columns (get-reflection-row replaced-smudge-board)]
        (if (empty? reflection-columns)
          nil
          (-> (set (filter #(is-valid-reflection? replaced-smudge-board %) reflection-columns))
              (set/difference (set old-reflection-columns))
              first))))))

(defn valid-reflections [board]
  (let [replaced-smudge-boards (replace-smudge-in-row board)]
    (->> (map #(find-valid-reflection board %) replaced-smudge-boards)
         (remove nil?)
         first)))

(defn part-2 []
  (->> (map #(array-map :horizontal (valid-reflections %)
                        :vertical  (valid-reflections (rotate-board %))) data)
       (reduce (fn [acc m]
                 (cond
                   (nil? (:vertical m))
                   (+ acc (* 100 (inc (:horizontal m))))

                   :else
                   (+ acc (inc (:vertical m)))))
               0)))