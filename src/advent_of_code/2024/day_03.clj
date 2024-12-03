(ns advent-of-code.2024.day-03
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.string :as str]))

(def test-data "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def test-data-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))\n")
(def data (apply str (read/read-file "resources/2024/day_03.txt")))

(defn find-multiplies [regex input]
  (->> (re-seq regex input)
       (map first)))

(defn multiply [input]
  (-> (re-seq #"\d+,\d+" input)
      first
      (str/split #",")
      (utils/parse-int-col)
      (->> (apply *))))

(defn part-1 [input]
  (->> (find-multiplies #"(mul\(\d+,\d+\))" input)
       (map multiply)
       (reduce +)))

(defn part-2 [input]
  (loop [instructions (find-multiplies #"(mul\(\d+,\d+\)|do\(\)|don't\(\))" input)
         enabled? true
         acc 0]
    (if (empty? instructions)
      acc
      (cond
        (= "don't()" (first instructions))
        (recur (rest instructions) false acc)

        (= "do()" (first instructions))
        (recur (rest instructions) true acc)

        enabled?
        (recur (rest instructions) enabled? (+ acc (multiply (first instructions))))

        :else
        (recur (rest instructions) enabled? acc)))))



