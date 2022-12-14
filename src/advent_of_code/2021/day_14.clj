(ns advent-of-code.2021.day-14
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def data (into {} (map #(str/split % #" -> ") (read/read-file "resources/day_14.txt"))))

(defn expand-polymer [form amount]
  (let [found (get data form)]
    {(str (first form) found)  amount
     (str found (second form)) amount}))

(defn get-polymer-frequencies [[polymer freqs]]
  (loop [p    polymer
         freq freqs
         acc  {}]
    (if (empty? p)
      [acc freq]
      (let [[form amount] (first p)
            found             (get data form)
            next-polymer-freq (expand-polymer form amount)]
        (recur (rest p)
               (merge-with + freq {found amount})
               (merge-with + acc next-polymer-freq))))))

(defn iterate-polymer [input iterate-count]
  (let [char-frequencies (frequencies (map str input))
        initial-polymer (frequencies (map #(apply str %) (partition 2 1 input)))]
    (first (->> (iterate get-polymer-frequencies [initial-polymer char-frequencies])
                (drop iterate-count)
                (take 1)))))

(defn part-1 [input]
  (let [[_ cs] (iterate-polymer input 10)
        minimum (second (apply min-key val cs))
        maximum (second (apply max-key val cs))]
    (- maximum minimum)))

(defn part-2 [input]
  (let [[_ cs] (iterate-polymer input 40)
        minimum (second (apply min-key val cs))
        maximum (second (apply max-key val cs))]
    (- maximum minimum)))