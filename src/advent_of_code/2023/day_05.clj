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


(def seed-ranges (->> (first data)
                      :values
                      (partition 2)
                      (map (fn [[start len]] [start (+ start (dec len))]))))

(defn parse-line [line]
  (let [[dest src rng] (read/integers line)]
    {:lo   src
     :hi   (+ src (dec rng))
     :diff (- dest src)}))

(defn parse-maps [maps]
  (->> (rest maps)
       (map parse-line)
       (sort-by :lo)))

(defn run-seed [rule-maps]
  (loop [seeds seed-ranges
         rules rule-maps
         result []]
    (let [_ (println rules)
          [start stop] (first seeds)
          [destination source length] (first rules)
          high (+ source (dec length))
          low source
          difference (- destination source)]
      (if (or (empty? seeds) (empty? rules))
        (sort-by :start (into result seeds))
        (cond
          (> start high) (recur seeds (rest rules) result)
          (< stop low) (recur (rest seeds)
                              rules
                              (conj result {:start start
                                            :stop  stop}))
          (<= low start stop high) (recur (rest seeds)
                                          rules
                                          (conj result {:start (+ difference start)
                                                        :stop  (+ difference stop)}))
          (<= low start high stop) (recur (conj (rest seeds) {:start (inc high)
                                                              :stop  stop})
                                          (rest rules)
                                          (conj result {:start (+ difference start)
                                                        :stop  (+ difference high)}))
          (<= start low stop high) (recur (rest seeds)
                                         rules
                                         (-> result
                                             (conj {:start start
                                                    :stop  (dec low)})
                                             (conj {:start (+ difference low)
                                                    :stop  (+ difference stop)}))))))))


(defn part-2 []
  (let [[seeds & maps] (read/read-input-paragraphs "2023/day_05")]
    [seeds (map parse-maps maps)]))
