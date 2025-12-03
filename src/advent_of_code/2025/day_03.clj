(ns advent-of-code.2025.day-03
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def test-bank [9 8 7 6 5 4 3 2 1 1 1 1 1 1 1])
(def test-bank-2 [8 1 1 1 1 1 1 1 1 1 1 1 1 1 9])
(def test-bank-3 [2 3 4 2 3 4 2 3 4 2 3 4 2 7 8])
(def test-bank-4 [8 1 8 1 8 1 9 1 1 1 1 2 1 1 1])
(def test-data [test-bank test-bank-2 test-bank-3 test-bank-4])

(def data (->> (read/read-file "resources/2025/day_03.txt")
               (mapv #(->> (str/split % #"")
                           (mapv (fn [n] (Integer/parseInt n)))))))


(defn drop-index
  [coll index]
  (into (subvec coll 0 index) (subvec coll (inc index))))

(defn first-smaller-index [bank]
  (loop [pointer 0]
    (if (>= (inc pointer) (count bank))
      nil
      (if (< (get bank pointer) (get bank (inc pointer)))
        pointer
        (recur (inc pointer))))))

(defn drop-to [coll amt]
  (loop [bank coll]
    (if (= amt (count bank))
      bank
      (let [fsi (first-smaller-index bank)]
        (cond
          ;; The list is ordered and I can take the first (amt)
          (nil? fsi)
          (take amt bank)

          :else
          (recur (drop-index bank fsi)))))))

(defn part-1 []
  (->> (map #(drop-to % 2) data)
       (map #(apply str %))
       (map #(BigInteger. %))
       (reduce +)))

(defn part-2 []
  (->> (map #(drop-to % 12) data)
       (map #(apply str %))
       (map #(BigInteger. %))
       (reduce +)))


