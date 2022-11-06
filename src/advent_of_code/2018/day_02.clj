(ns advent-of-code.2018.day-02
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.data :as data]))

(defn contains-amount-of-characters [s n]
  (some #(= % n) (vals (frequencies s))))

(defn part-1 []
  (let [data (read/read-file "resources/2018/day_02.txt")
        has-characters-col (map #(vector (contains-amount-of-characters % 2)
                                         (contains-amount-of-characters % 3)) data)]
    (reduce * [(count (filter true? (map first has-characters-col)))
               (count (filter true? (map second has-characters-col)))])))

(defn is-off-by-one? [s1 s2]
  (->> (map (fn [c1 c2] (= c1 c2)) s1 s2)
       (filter false?)
       count
       (= 1)))

(defn find-is-off-by-one? [s col]
  (filter #(is-off-by-one? s %) col))

(defn common-letters [s1 s2]
  (->> (map #(vector (= %1 %2) %1) s1 s2)
       (filter (fn [[exists? _]] exists?))
       (map second)
       (apply str)))

;(defn- common-letters [s1 s2]
;  (->> (map vector s1 s2)
;       (filter #(apply = %))
;       (map first)
;       (apply str)))

;(defn- common-letters [s1 s2]
;  (->> (data/diff (seq s1) (seq s2))
;       last
;       (apply str)))

(defn part-2 []
  (let [data (read/read-file "resources/2018/day_02.txt")]
    (loop [check (first data)
           left (rest data)]
      (let [found (find-is-off-by-one? check left)]
        (if (seq found)
          (common-letters check (first found))
          (recur (first left) (rest left)))))))


