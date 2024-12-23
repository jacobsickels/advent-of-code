(ns advent-of-code.2024.day-23
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(require '[clojure.data.priority-map :refer [priority-map]])

(def data (->> (read/read-file "resources/2024/day_23.txt")
               (map #(str/split % #"-"))))

(defn get-lan-map []
  (loop [connections data
         acc {}]
    (if (empty? connections)
      acc
      (let [connection (first connections)]
        (recur (rest connections) (assoc acc (first connection) (conj (get acc (first connection) #{}) (second connection))
                                             (second connection) (conj (get acc (second connection) #{}) (first connection))))))))

(defn generate-first-permutations [computers lan-map]
  (let [unique-computers (set (keys lan-map))]
    (->> (set/difference unique-computers computers)
         (map #(conj computers %)))))

(defn generate-second-permutations [computers lan-map]
  (let [unique-computers (set (keys lan-map))
        perms (generate-first-permutations computers lan-map)]
    (set (mapcat #(->> (set/difference unique-computers %)
                       (map (fn [c] (conj % c)))) perms))))


(defn are-computers-connected? [computers lan-map]
  (every? #(set/subset? (set/difference computers #{%}) (get lan-map %)) computers))

(defn connection-has-t? [connections]
  (->> (filter #(str/starts-with? % "t") connections)
       (not-empty)))


(defn part-1 []
  (let [lan-map (get-lan-map)
        unique-computers (set (keys lan-map))
        check-connections (set (mapcat #(generate-second-permutations #{%} lan-map) unique-computers))]
    (->> (filter #(are-computers-connected? % lan-map) check-connections)
         (filter connection-has-t?)
         (count))))

;; =====
(defn get-lan-map-2 []
  (loop [connections data
         acc {}]
    (if (empty? connections)
      acc
      (let [connection (first connections)]
        (recur (rest connections) (assoc acc (first connection) (assoc (get acc (first connection)) (second connection) 1)
                                             (second connection) (assoc (get acc (second connection)) (first connection) 1)))))))

(defn get-groups [check lan-map]
  (loop [v (keys (get lan-map check))
         groups [#{}]]
    (if (empty? v)
      (->> (map #(conj % check) groups)
           (sort-by count)
           (last))
      (let [next-level (keys (get lan-map (first v)))]
        (recur
          (rest v)
          (reduce (fn [acc g]
                    (let [diff (set/intersection g (set next-level))]
                      (cond
                        (= g diff)
                        (conj acc (conj g (first v)))

                        (empty? diff)
                        (conj acc g #{(first v)})

                        :else acc)))
                  []
                  groups))))))

(defn part-2 []
  (let [lan-map (get-lan-map-2)
        unique-computers (set (keys lan-map))]
    (->> (map #(get-groups % lan-map) unique-computers)
         (sort-by count)
         (partition-by count)
         (last)
         set
         (first)
         (sort)
         (str/join ","))))

