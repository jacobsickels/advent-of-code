(ns advent-of-code.2022.day-23
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [advent-of-code.shared.point :as points]))

(def original-direction-order {:north :south :south :west :west :east :east :north})

(def data (->> (read/read-file "resources/2022/day_23.txt")
               (map-indexed (fn [i line]
                              (map-indexed
                                (fn [j space] (list [j i] space))
                                (str/split line #""))))
               (apply concat)
               (remove #(= (second %) "."))
               (map #(hash-map :position (first %) :direction-pos :north))
               set))

(defn points-contain-elves? [points elves]
  (not (empty? (filter #(contains? elves %) points))))

(defn elves-to-north? [[x y] elves]
  (points-contain-elves? [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]] elves))

(defn elves-to-south? [[x y] elves]
  (points-contain-elves? [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]] elves))

(defn elves-to-east? [[x y] elves]
  (points-contain-elves? [[(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]] elves))

(defn elves-to-west? [[x y] elves]
  (points-contain-elves? [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]] elves))

(defn propose-new-pos [elf elves]
  (let [[x y] (:position elf)
        direction-pos (:direction-pos elf)
        around (points/points-around [x y])]
    (if (not (points-contain-elves? around elves))
      [:none [x y]]
      (loop [check-pos direction-pos
             checked 0]
        (cond
          (= 4 checked)
          [:none [x y]]

          ;; NE N NW
          (and (= check-pos :north) (not (elves-to-north? [x y] elves)))
          [:north [x (dec y)]]

          ;; SE S SW
          (and (= check-pos :south) (not (elves-to-south? [x y] elves)))
          [:south [x (inc y)]]

          ;; NW W SW
          (and (= check-pos :west) (not (elves-to-west? [x y] elves)))
          [:west [(dec x) y]]

          ;; east
          (and (= check-pos :east) (not (elves-to-east? [x y] elves)))
          [:east [(inc x) y]]

          :else (recur (get original-direction-order check-pos) (inc checked)))))))

(defn make-propositions [elves]
  (let [elf-positions (set (map :position elves))]
    (map #(assoc % :proposition (propose-new-pos % elf-positions)) elves)))

(defn make-moves [elves-with-propositions]
  (let [unique-propositions (frequencies (map #(second (:proposition %)) elves-with-propositions))]
    (map #(if (= 1 (get unique-propositions (second (:proposition %))))
            (-> (assoc % :position (second (:proposition %))
                         :direction-pos (get original-direction-order (:direction-pos %)))
              (dissoc  :proposition))
            (-> (assoc % :direction-pos (get original-direction-order (:direction-pos %)))
                (dissoc :proposition)))
         elves-with-propositions)))

(defn move-rounds [elves]
  (loop [rounds 0
         positions elves]
    (if (= rounds 10)
      positions
      (recur (inc rounds) (make-moves (make-propositions positions))))))

(defn part-1 [elves]
  (let [positions (map :position (move-rounds elves))
        min-x (ffirst (sort-by first positions))
        max-x (first (last (sort-by first positions)))
        min-y (last (first (sort-by second positions)))
        max-y (last (last (sort-by second positions)))]
    (- (* (- (inc max-x) min-x) (- (inc max-y) min-y)) (count positions))))

(defn move-rounds-to-end [elves]
  (loop [rounds 0
         elf-positions elves
         previous-positions #{}]
    (if (= (set (map :position elf-positions)) previous-positions)
      rounds
      (let [next-elves (make-moves (make-propositions elf-positions))]
        (recur (inc rounds) next-elves (set (map :position elf-positions)))))))

(defn part-2 [elves]
  (move-rounds-to-end elves))

