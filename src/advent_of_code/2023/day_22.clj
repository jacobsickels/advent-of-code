(ns advent-of-code.2023.day-22
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))


(def data (->> (read/read-file "resources/2023/day_22.txt")
               (map #(let [[f s] (str/split % #"~")
                           [x1 y1 z1] (map (fn [c] (Integer/parseInt c)) (str/split f #","))
                           [x2 y2 z2] (map (fn [c] (Integer/parseInt c)) (str/split s #","))]
                       [[x1 y1 z1] [x2 y2 z2]]))))

(defn points-between-3d [block]
  (let [[[x1 y1 z1] [x2 y2 z2]] block]
    (cond
      (and (= x1 x2) (= y1 y2))
      (map #(vec [x1 y1 %]) (apply utils/inclusive-range (sort [z1 z2])))

      (and (= x1 x2) (= z1 z2))
      (map #(vec [x1 % z1]) (apply utils/inclusive-range (sort [y1 y2])))

      (and (= y1 y2) (= z1 z2))
      (map #(vec [% y1 z1]) (apply utils/inclusive-range (sort [x1 x2]))))))

(defn get-block-points [block]
  (points-between-3d block))

(def memoized-get-block-points (memoize get-block-points))

(defn descended-block-points [block return-block?]
  (let [block-points (mapv (fn [[x y z]] [x y (dec z)]) (memoized-get-block-points block))]
    (if return-block? [(first block-points) (last block-points)] block-points)))

(defn get-block-height [block]
  (apply min (map last block)))

(defn is-block-rested? [block rested-blocks]
  (let [check-descended (descended-block-points block false)
        rested-block-points (set (mapcat #(memoized-get-block-points %) rested-blocks))]
    (cond
      (not (empty? (set/intersection rested-block-points (set check-descended))))
      true

      :else (zero? (get-block-height check-descended)))))

(defn blocks-to-rest [blocks]
  (loop [col (sort-by get-block-height blocks)
         check-block (first col)
         rested #{}]
    (println (count col))
    (if (empty? col)
      rested
      (let [is-rested? (is-block-rested? check-block rested)]
        (cond
          is-rested?
          (recur (rest col)
                 (first (rest col))
                 (conj rested check-block))



          :else (recur col
                       (descended-block-points check-block true)
                       rested))))))

(defn find-support [block rested-blocks]
  (let [block-points (descended-block-points block false)
        check-blocks (disj rested-blocks block)
        support-blocks (->> check-blocks
                            (filter #(not (empty? (set/intersection (set (memoized-get-block-points %)) (set block-points))))))]
    (if (empty? support-blocks) :ground support-blocks)))

(defn find-supports [rested-blocks]
  (map #(array-map :block % :supports (find-support % rested-blocks)) rested-blocks))

(defn can-block-be-removed? [block rested-block-supports]
  (->>
    (remove #(= block (:block %)) rested-block-supports)
    (map :supports)
    (map #(if (= :ground %)
            [:ground]
            (remove (fn [support-block] (= support-block block)) %)))
    (filter empty?)
    empty?))

(defn part-1 []
  (let [rested-blocks (blocks-to-rest data)
        supported-blocks (find-supports rested-blocks)]
    (frequencies (map #(can-block-be-removed? (:block %) supported-blocks) supported-blocks))))


(defn collapsed-blocks [block rested-block-supports]
  (let [next-supports (->>
                        (remove #(= block (:block %)) rested-block-supports)
                        (map #(if (= :ground (:supports %))
                                %
                                (assoc % :supports (remove (fn [support-block] (= support-block block)) (:supports %))))))]
    [next-supports (->> (filter #(if (= :ground (:supports %))
                                   false
                                   (empty? (:supports %))) next-supports)
                        (map :block))]))
(defn count-collapsed [removed-block support-blocks]
  (loop [supports support-blocks
         to-remove #{removed-block}
         cnt 0]
    (if (empty? to-remove)
      (dec cnt)
      (let [block-to-remove (first to-remove)
            [next-supports next-to-remove] (collapsed-blocks block-to-remove supports)]
        (recur next-supports (disj (set/union to-remove (set next-to-remove)) block-to-remove) (inc cnt))))))

(defn part-2 []
  (let [rested-blocks (blocks-to-rest data)
        supported-blocks (find-supports rested-blocks)
        check-removal (->> (map #(vector (:block %) (can-block-be-removed? (:block %) supported-blocks)) supported-blocks)
                           (filter #(false? (second %)))
                           (map first))]
    (map #(count-collapsed % supported-blocks) check-removal)))

