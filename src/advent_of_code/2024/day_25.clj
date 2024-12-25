(ns advent-of-code.2024.day-25
  (:require [advent-of-code.shared.read-file :as read]))

(def data (->> (read/read-file "resources/2024/day_25.txt")
               (partition-by empty?)))

(defn rotate [col]
  (->> (map (fn [n] (map #(nth % n) col))
            (range 0 (count (first col))))
       (map #(apply str %))))


(def lock-list (->> (filter #(= (ffirst %) \#) data)
                    (map rotate)
                    (map #(->> (map frequencies %)
                               (map (fn [freq-map] (dec (get freq-map \#))))))))

(def key-list (->> (filter #(= (first (last %)) \#) data)
                   (map rotate)
                   (map #(->> (map frequencies %)
                              (map (fn [freq-map] (dec (get freq-map \#))))))))

(defn do-lock-and-key-fit? [lock key]
  (->> (map + lock key)
       (every? #(>= 5 %))))


(defn part-1 []
  (loop [locks lock-list
         keys key-list
         founds #{}]
    (if (empty? locks)
      (count founds)
      (if (empty? keys)
        (recur (rest locks) key-list founds)
        (let [fits? (do-lock-and-key-fit? (first locks) (first keys))]
          (if fits?
            (recur locks (rest keys) (conj founds {:lock (first locks) :key (first keys)}))
            (recur locks (rest keys) founds)))))))