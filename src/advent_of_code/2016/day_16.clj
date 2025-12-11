(ns advent-of-code.2016.day-16
  (:require [clojure.string :as str]))

(defn next-state [input]
  (let [a input
        b (str/replace (apply str (reverse input)) #"0|1" {"0" "1" "1" "0"})]
    (str a "0" b)))

(defn get-dragon-string [input length]
  (loop [i input]
    (if (<= length (count i))
      (apply str (take length i))
      (recur (next-state i)))))

(defn check-sum [input]
  (if (even? (count input))
    (->> (partition 2 input)
         (map #(if (apply = %) \1 \0))
         (apply str)
         (check-sum))
    input))

(defn fill-disk [input length]
  (->> (get-dragon-string input length)
       (check-sum)))

(defn part-1 []
  (fill-disk "11100010111110100" 272))

(defn part-2 []
  (fill-disk "11100010111110100" 35651584))