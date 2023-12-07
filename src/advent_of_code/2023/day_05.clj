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

(defn includes? "returns true if x is in range"
  [[fst lst incl cmp] x]
  (let [cmp (or cmp compare)]
    (and (<= (cmp fst x) 0) ((if incl <= <) (cmp x lst) 0))))

(defn overlaps? "returns true if rt least 2 ranges overlap"
  [[fst1 _ _ _ :as r1] [fst2 _ _ _ :as r2] & rest]
  (if (empty? rest)
    (or (includes? r2 fst1) (includes? r1 fst2))
    (or (overlaps? r1 r2) (some #(overlaps? r1 %) rest) (some #(overlaps? r2 %) rest))))

;; https://github.com/narimiran/AdventOfCode2023/blob/main/clojure/day05.clj

(def seed-ranges (->> (first data)
                      :values
                      (partition 2)
                      (map (fn [[start len]] {:start start :stop (+ start (dec len))}))
                      (sort-by :start)))

(def part-2-data (->> (map :values (rest data))
                      (map (fn [value-vectors]
                             (->> (map (fn [[destination source length]]
                                         {:lo   source
                                          :hi   (+ source (dec length))
                                          :diff (- destination source)})
                                       value-vectors)
                                  (sort-by :lo))))))

(defn convert-2 [srcs rules]
  (loop [[{:keys [start stop]} & rem-srcs :as srcs] srcs
         [{:keys [lo hi diff]} & rem-rules :as rules] rules
         result []]
    (if (or (empty? srcs) (empty? rules))
      (sort-by :start (into result srcs))
      (cond
        (> start hi)          (recur srcs rem-rules result)
        (< stop lo)           (recur rem-srcs
                                     rules
                                     (conj result {:start start
                                                   :stop  stop}))
        (<= lo start stop hi) (recur rem-srcs
                                     rules
                                     (conj result {:start (+ diff start)
                                                   :stop  (+ diff stop)}))
        (<= lo start hi stop) (recur (conj rem-srcs {:start (inc hi)
                                                     :stop  stop})
                                     rem-rules
                                     (conj result {:start (+ diff start)
                                                   :stop  (+ diff hi)}))
        (<= start lo stop hi) (recur rem-srcs
                                     rules
                                     (-> result
                                         (conj {:start start
                                                :stop  (dec lo)})
                                         (conj {:start (+ diff lo)
                                                :stop  (+ diff stop)})))))))


