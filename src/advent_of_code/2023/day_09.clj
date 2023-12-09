(ns advent-of-code.2023.day-09
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2023/day_09.txt")
               (map #(let [number-strings (str/split % #" ")]
                       (mapv (fn [num-str] (Integer/parseInt num-str)) number-strings)))))

(defn get-differences [col]
  (loop [check col
         result []]
    (if (= 1 (count check))
      result
      (recur (rest check) (conj result (- (second check) (first check)))))))

(defn get-pyramid [col]
  (loop [result [col]]
    (if (= #{0} (set (last result)))
      result
      (recur (conj result (get-differences (last result)))))))

(defn get-next-pyramid [pyramid]
  (loop [check (reverse pyramid)
         result []]
    (if (empty? check)
      result
      (cond
        (= #{0} (set (first check)))
        (recur (rest check) (conj result (conj (first check) 0)))

        (= 1 (count (set (first check))))
        (recur (rest check) (conj result (conj (first check) (ffirst check))))


        :else
        (recur (rest check)
               (conj result (conj (first check) (+ (last (last result)) (last (first check))))))))))

(defn part-1 []
  (->> (map get-pyramid data)
       (map get-next-pyramid)
       (map #(last (last %)))
       (reduce +)))

(defn get-next-pyramid-2 [pyramid]
  (loop [check (reverse pyramid)
         result []]
    (if (empty? check)
      result
      (cond
        (= #{0} (set (first check)))
        (recur (rest check) (conj result (conj (first check) 0)))

        (= 1 (count (set (first check))))
        (recur (rest check) (conj result (conj (first check) (ffirst check))))

        :else
        (recur (rest check)
               (conj result (cons (- (first (first check)) (first (last result))) (first check))))))))

(defn part-2 []
  (->> (map get-pyramid data)
       (map get-next-pyramid-2)
       (map #(first (last %)))
       (reduce +)))



