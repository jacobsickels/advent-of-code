(ns advent-of-code.2024.day-23
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_23.txt")
               (map #(str/split % #"-"))))

(defn get-lan-map []
  (loop [connections data
         acc {}]
    (if (empty? connections)
      acc
      (let [connection (first connections)]
        (recur (rest connections)
               (assoc acc (first connection) (conj (get acc (first connection) #{}) (second connection))
                          (second connection) (conj (get acc (second connection) #{}) (first connection))))))))

(def lan-map (get-lan-map))

(defn are-computers-connected? [computers lan-map]
  (every? #(set/subset? (set/difference computers #{%}) (get lan-map %)) computers))

(defn connection-has-t? [connections]
  (->> (filter #(str/starts-with? % "t") connections)
       (not-empty)))

(defn permutations []
  (->> (mapcat (fn [[k v]] (combo/cartesian-product #{k} v)) lan-map)
       (mapcat (fn [[k v]] (map #(conj % k) (combo/cartesian-product #{v} (get lan-map v)))))
       (map set)
       set
       (filter #(= (count %) 3))))

(defn part-1 []
  (let [perms (permutations)]
    (->> (filter #(are-computers-connected? % lan-map) perms)
         (filter connection-has-t?)
         (count))))

(defn get-groups [check]
  (loop [v (get lan-map check)
         groups [#{}]]
    (if (empty? v)
      (->> (map #(conj % check) groups)
           (sort-by count)
           (last))
      (let [next-level (get lan-map (first v))]
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
  (let [unique-computers (set (keys lan-map))]
    (->> (map #(get-groups %) unique-computers)
         (sort-by count)
         (partition-by count)
         (last)
         set
         (first)
         (sort)
         (str/join ","))))

