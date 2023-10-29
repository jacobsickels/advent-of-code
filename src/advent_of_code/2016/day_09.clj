(ns advent-of-code.2016.day-09
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def data (first (read/read-file "resources/2016/day_09.txt")))

(defn decompress [col]
  (let [[to-remove marker] (re-find #"\(([^\)]+)\)" col)
        [num-characters num-repeat] (map #(Integer/parseInt %) (str/split marker #"x"))]
    [(+ (count to-remove) num-characters) (apply str (repeat num-repeat (apply str (take num-characters (drop (count to-remove) col)))))]))

(defn run-decompression [col] ;; 150914
  (loop [check col
         result ""
         decompressed? false]
    (cond
      (empty? check)
      [result decompressed?]

      (= \( (first check))
      (let [[num-to-drop decompressed] (decompress check)]
        (recur (subs check num-to-drop) (str result decompressed) true))

      :else
      (recur (subs check 1) (str result (first check)) decompressed?))))

(defn part-1 [col]
  (count (first (run-decompression col))))

;; Doesn't work for data set, too large
(defn part-2 [col]
  (loop [check col]
    (let [[result decompressed?] (run-decompression check)]
      (if (not decompressed?)
        (count result)
        (recur result)))))
