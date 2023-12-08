(ns advent-of-code.2023.day-05
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(defn parse [line]
  (let [l (str/join " " line)
        [map-type numbers] (str/split l #": ")]
    (if (= map-type "seeds")
      {:type   "seed"
       :values (map #(BigInteger. %) (str/split numbers #" "))}
      {:type   (-> (first (str/split map-type #" "))
                   (str/split #"-"))
       :values (->> (str/split numbers #" ")
                    (map #(BigInteger. %))
                    (partition 3))})))

(def data (->> (read/read-file "resources/2023/day_05.txt")
               (partition-by empty?)
               (remove #(empty? (first %)))
               (map parse)))

(defn lookup-value [value value-map]
  (let [lookup (->> (map (fn [[destination-start source-start range-length]]
                           (if (<= source-start value (+ source-start range-length))
                             (+ value (- destination-start source-start))
                             nil))
                         value-map)
                    (remove nil?)
                    first)]
    (if (nil? lookup) value lookup)))


(defn translate [input col]
  (let [source-type (:type input)
        values (:values input)
        lookup (first (filter #(= (first (:type %)) source-type) col))]
    {:type   (last (:type lookup))
     :values (map (fn [value] (lookup-value value (:values lookup))) values)}))

(defn get-lowest-location [seed-data]
  (first (sort (:values (loop [col seed-data]
                          (if (= "location" (:type col))
                            col
                            (recur (translate col data))))))))

(defn part-1 []
  (get-lowest-location (first data)))

(defn translate-to-range [value-range]
  (let [[destination-start source-start range-length] value-range]
    [[source-start (+ source-start (dec range-length))] (- destination-start source-start)]))

(def data-2 (->> (map #(assoc %
                         :type (last (:type %))
                         :values (->> (map translate-to-range (:values %))
                                      (sort-by ffirst))) (rest data))))

(def seed-ranges (->> (first data)
                      :values
                      (partition 2)
                      (map (fn [[start len]] [start (+ start (dec len))]))
                      (sort-by first)))

(defn includes? "returns true if x is in range"
  [[fst lst incl cmp] x]
  (let [cmp (or cmp compare)]
    (and (<= (cmp fst x) 0) ((if incl <= <) (cmp x lst) 0))))

(defn overlaps? "returns true if rt least 2 ranges overlap"
  [[fst1 _ _ _ :as r1] [fst2 _ _ _ :as r2] & rest]
  (if (empty? rest)
    (or (includes? r2 fst1) (includes? r1 fst2))
    (or (overlaps? r1 r2) (some #(overlaps? r1 %) rest) (some #(overlaps? r2 %) rest))))

(defn translate-seed [seed-range translation-values]
  (let [[translation-range difference] translation-values]
    (if (overlaps? seed-range translation-range)
      (let [[sx sy] seed-range
            [tx ty] translation-range]
        (println [sx sy] [tx ty])
        {:overlapped? true :result (cond
                                     (< sx tx sy ty)
                                     [[sx (dec tx)] [(+ difference tx) (+ sy difference)]]

                                     (< tx sx ty sy)
                                     [[(+ difference sx) (+ difference ty)] [(inc ty) sy]]

                                     (< sx tx ty sy)
                                     [[sx (dec tx)] [(+ difference tx) (+ difference ty)] [(inc ty) sy]]

                                     (< tx sx sy ty)
                                     [[(+ difference sx) (+ difference sy)]]

                                     :else [seed-range])})
      {:overlapped? false :result [seed-range]})))

(defn seed-through-ranges [seed-range ranges]
  (let [results (map #(translate-seed seed-range %) ranges)]
    (println results)
    (if (zero? (count (filter :overlapped? results)))
      [seed-range]
      (->> (filter :overlapped? results)
           (map :result)
           (apply concat)))))

(defn translate-seeds [input-seed-ranges translate-key]
  (let [translation-values (first (filter #(= translate-key (:type %)) data-2))]
    (loop [seeds input-seed-ranges
           result []]
      (if (empty? seeds)
        (sort-by first result)
        (recur
          (rest seeds)
          (concat result (seed-through-ranges (first seeds) (:values translation-values))))))))

(defn part-2 []
  (-> (translate-seeds seed-ranges "soil")
      (translate-seeds "fertilizer")
      (translate-seeds "water")
      (translate-seeds "light")))