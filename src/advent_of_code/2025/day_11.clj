(ns advent-of-code.2025.day-11
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def graph (->> (read/read-file "resources/2025/day_11.txt")
                (reduce (fn [acc line]
                          (let [[k v] (str/split line #": ")]
                            (assoc acc (keyword k) (map keyword (str/split v #" "))))) {})))

(defn get-path-count [graph start end]
  (loop [path-count (merge (->> (concat (keys graph) (mapcat (fn [[_ v]] v) graph))
                                set
                                (reduce (fn [acc k] (assoc acc k nil)) {}))
                           {end 1})]
    ;; Once we get to the start node we have summed up all the paths
    (if (not (nil? (get path-count start)))
      (get path-count start)
      (recur (reduce (fn [acc [k v]]
                       (let [children-vals (map #(get path-count %) (get graph k))]
                         ;; If current node is nil but every child has a value, replace with sum
                         (if (and (nil? (get path-count k)) (every? number? children-vals))
                           (assoc acc k (reduce + children-vals))
                           (assoc acc k v))))
                     {} path-count)))))

(defn part-1 [] (get-path-count graph :you :out))

;; :dac to :fft returned zero in my data
(defn part-2 []
  (* (get-path-count graph :svr :fft)
     (get-path-count graph :fft :dac)
     (get-path-count graph :dac :out)))