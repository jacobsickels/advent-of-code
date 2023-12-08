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
  (let [[translation-range difference] translation-values
        [sx sy] seed-range
        [tx ty] translation-range]
    (cond
      (<= sx tx sy ty)
      (cond
        (= sx tx)
        {:translated [[(+ difference tx) (+ sy difference)]] :carry [] :overlaps :right}

        :else
        {:translated [[(+ difference tx) (+ sy difference)]] :carry [[sx (dec tx)]] :overlaps :right})

      (<= tx sx ty sy)
      (cond
        (= ty sy)
        {:translated [[(+ difference sx) (+ difference ty)]] :carry [] :overlaps :left}

        :else
        {:translated [[(+ difference sx) (+ difference ty)]] :carry [[(inc ty) sy]] :overlaps :left})

      (<= sx tx ty sy)
      {:translated [[(+ difference tx) (+ difference ty)]] :carry [[sx (dec tx)] [(inc ty) sy]] :overlaps :inside}

      (<= tx sx sy ty)
      {:translated [[(+ difference sx) (+ difference sy)]] :carry [] :overlaps :outside}

      :else
      {:translated [] :carry [seed-range] :overlaps :none})))

(defn seed-through-ranges [seed-range ranges]
  (let [results (map #(translate-seed seed-range %) ranges)]
    (if (empty? (remove #(= :none (:overlaps %)) results))
      [seed-range]
      (let [translations (->> (remove #(= :none (:overlaps %)) results))
            lr-overlap? (->> (filter #(contains? #{:left :right} (:overlaps %)) translations)
                             (map :overlaps)
                             set
                             (= #{:left :right}))]
        (->> (mapcat (fn [translation]
                       (cond
                         (= :outside (:overlaps translation))
                         (:translated translation)

                         (and lr-overlap? (= :left (:overlaps translation)))
                         (:translated translation)

                         (= :left (:overlaps translation))
                         (concat (:translated translation) (:carry translation))

                         (and lr-overlap? (= :right (:overlaps translation)))
                         (:translated translation)

                         (= :right (:overlaps translation))
                         (concat (:translated translation) (:carry translation))

                         (= :inside (:overlaps translation))
                         (:translated translation)

                         :else (:carry translation)))
                  translations)
             (sort-by first))))))

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
      (translate-seeds "light")
      (translate-seeds "temperature")
      (translate-seeds "humidity")
      (translate-seeds "location")
      ffirst))