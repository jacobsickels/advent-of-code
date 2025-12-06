(ns advent-of-code.2025.day-05
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def ranges (->> (read/read-file "resources/day_05_ranges.txt")
                 (map #(map (fn [n] (BigInteger. n)) (str/split % #"-")))))

(def ids (->> (read/read-file "resources/day_05_ids.txt")
              (map #(BigInteger. %))))

(defn is-in-range? [[n1 n2] n] (<= n1 n n2))

(defn is-in-a-range? [n]
  (not (nil? (some #(is-in-range? % n) ranges))))

(defn part-1 []
  (frequencies (map is-in-a-range? ids)))

(defn should-merge? [range check-range]
  (let [[r1 r2] range
        [c1 c2] check-range]
    (<= r1 c1 r2)))

(defn merge-range [ranges [c1 c2]]
  (let [[r1 r2] (last ranges)]
    (conj (vec (butlast ranges)) [r1 (max r2 c2)])))

(defn merge-whole-range []
  (let [sorted (sort-by first ranges)]
    (loop [r (rest sorted)
           acc [(first sorted)]]
      (if (empty? r)
        acc
        (let [should-merge (should-merge? (last acc) (first r))]
          (cond
            should-merge
            (recur (rest r) (sort-by first (merge-range acc (first r))))

            :else
            (recur (rest r) (sort-by first (conj acc (vec (first r)))))))))))

(defn part-2 []
  (->> (map (fn [[n1 n2]] (inc (- n2 n1))) (merge-whole-range))
       (reduce +)))