(ns advent-of-code.2024.day-19
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_19.txt")
               (partition-by empty?)))

(def patterns (set (str/split (ffirst data) #", ")))
(def designs (set (last data)))

(defn design-starts-with-patterns [design]
  (loop [check-patterns patterns
         acc []]
    (if (empty? check-patterns)
      acc
      (if (str/starts-with? design (first check-patterns))
        (recur (rest check-patterns) (conj acc (first check-patterns)))
        (recur (rest check-patterns) acc)))))

(def memoized-design-starts-with-patterns (memoize design-starts-with-patterns))

;; ======================================

(defn translate-point-map [design point-map]
  (reduce (fn [acc [k v]] (merge-with + acc {(apply str (take k design)) v})) {} point-map))

(defn get-pattern-map [design index]
  (let [matches (memoized-design-starts-with-patterns (apply str (drop index design)))]
    (reduce (fn [acc match] (assoc acc (+ index (count match)) 1)) {} matches)))

(def memoized-get-pattern-map (memoize get-pattern-map))

(defn assign-count-to-map [pattern-map cnt]
  (reduce (fn [acc [k v]] (assoc acc k cnt)) {} pattern-map))

(defn iterate-point-map [design]
  (loop [pattern-map (memoized-get-pattern-map design 0)]
    (cond
      (empty? pattern-map)
      {}

      (= (set (keys pattern-map)) (set [(count design)]))
      pattern-map

      :else
      (recur (reduce (fn [acc [k v]]
                       (if (= (count design) k)
                         (merge-with + acc {k v})
                         (merge-with + acc (assign-count-to-map (memoized-get-pattern-map design k) v))))
                     {}
                     pattern-map)))))

(defn part-1 []
  (->> (map iterate-point-map designs)
       (remove empty?)
       count))

(defn part-2 []
  (->> (map iterate-point-map designs)
       (remove empty?)
       (map vals)
       flatten
       (reduce +)))














