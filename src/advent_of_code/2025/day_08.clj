(ns advent-of-code.2025.day-08
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2025/day_08.txt")))

(def points (->> (map #(->> (str/split % #",")
                            (map (fn [n] (Integer/parseInt n)))) data)))

(defn find-distance [[px1 py1 pz1] [px2 py2 pz2]]
  (Math/abs (Math/sqrt (+ (Math/pow (- px1 px2) 2)
                          (Math/pow (- py1 py2) 2)
                          (Math/pow (- pz1 pz2) 2)))))

(defn find-distance-pairs [points point]
  (let [filtered (remove #(= % point) points)]
    (apply merge (map #(hash-map (set [% point]) (find-distance point %)) filtered))))

(defn find-all-distance-pairs []
  (apply merge (map #(find-distance-pairs points %) points)))

(defn add-connection [connections pair]
  (let [has-intersection? (->> (map #(set/intersection % pair) connections)
                               (remove empty?)
                               empty?
                               not)
        new-connections (if has-intersection?
                          (map #(if (not-empty (set/intersection % pair))
                                  {:connections (set/union % pair) :merged true}
                                  {:connections % :merged false})
                               connections)
                          (conj (map #(hash-map :connections % :merged false) connections)
                                {:connections pair :merged false}))
        merged-connections (concat [(->> (filter #(true? (:merged %)) new-connections)
                                         (map :connections)
                                         (apply set/union))]
                                   (->> (filter #(false? (:merged %)) new-connections)
                                        (map #(set (:connections %)))))]
    (remove empty? merged-connections)))

(defn part-1 []
  (loop [distance-pairs (sort-by second (find-all-distance-pairs))
         connections []
         ittr 0]
    (if (= ittr 1000)
      (->> (map count connections)
           (sort)
           (take-last 3)
           (reduce *))
      (let [[shortest-pair _] (first distance-pairs)]
        (recur (rest distance-pairs) (add-connection connections shortest-pair) (inc ittr))))))

(defn was-fully-connected? [connections]
  (and (= 1 (count connections))
       (= (set points) (first connections))))

(defn part-2 []
  (loop [distance-pairs (sort-by second (find-all-distance-pairs))
         connections []
         fully-connected false]
    (if (not fully-connected)
      (let [[shortest-pair _] (first distance-pairs)
            new-connections (add-connection connections shortest-pair)
            fully-connected? (was-fully-connected? new-connections)]
        (if fully-connected?
          (->> (map first shortest-pair)
               (reduce *))
          (recur (rest distance-pairs) new-connections fully-connected?))))))