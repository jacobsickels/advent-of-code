(ns advent-of-code.2023.day-15
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(defn hash-algorithm [col]
  (reduce (fn [acc c]
            (rem (* 17 (+ acc (int c))) 256)) 0 col))

(def data (-> (read/read-file "resources/2023/day_15.txt")
              first
              (str/split #",")))

(def test-data (str/split "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" #","))

(defn part-1 []
  (->> (map hash-algorithm data)
       (reduce +)))

(defn parse-hash [col]
  (if (str/includes? col "=")
    (let [[f s] (str/split col #"=")]
      [(hash-algorithm f) f (Integer/parseInt s)])
    (let [[f s] (str/split col #"-")]
      [(hash-algorithm f) f nil])))

(defn put-in-boxes []
  (loop [d data
         results {}]
    (if (empty? d)
      results
      (let [[box-number label lens] (parse-hash (first d))]
        (if (nil? lens)
          (recur (rest d) (update-in results [box-number] #(->> (remove (fn [col] (= (first col) label)) %)
                                                                vec)))
          (recur (rest d) (update-in results [box-number] #(if (not-empty (filter (fn [col] (= (first col) label)) %))
                                                             (mapv (fn [col] (if (= (first col) label)
                                                                               [label lens]
                                                                               col))
                                                                  %)
                                                             (->> (conj % [label lens])
                                                                  vec)))))))))

(defn part-2 []
  (let [boxes (put-in-boxes)]
    (->> (map (fn [[k v]] (map-indexed (fn [i [label lens]] (* (inc k) (inc i) lens)) v))
              boxes)
         (flatten)
         (reduce +))))