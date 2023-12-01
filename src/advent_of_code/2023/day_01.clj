(ns advent-of-code.2023.day-01
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2023/day_01.txt")))
(def numbers {"1"     1
              "2"     2
              "3"     3
              "4"     4
              "5"     5
              "6"     6
              "7"     7
              "8"     8
              "9"     9
              "one"   1
              "two"   2
              "three" 3
              "four"  4
              "five"  5
              "six"   6
              "seven" 7
              "eight" 8
              "nine"  9})

(defn get-calibration-number [line digits]
  ; If characters overlap they can be replaced with separate words so the regex can work
  (let [parsed-line (str/replace line #"twone|oneight|threeight|fiveight|nineight|eightwo|eighthree|sevenine"
                                 {"twone"    "twoone"
                                  "oneight"   "oneeight"
                                  "threeight" "threeeight"
                                  "fiveight"  "fiveeight"
                                  "nineight"  "nineeight"
                                  "eightwo"   "eighttwo"
                                  "eighthree" "eightthree"
                                  "sevenine"  "sevennine"})
        calibration-numbers (re-seq (re-pattern (str "(?:" (str/join #"|" digits) ")")) parsed-line)]
    (+ (* 10 (get numbers (first calibration-numbers)))
       (get numbers (last calibration-numbers)))))

(defn part-1 []
  (let [digits (filter #(= 1 (count %)) (keys numbers))]
    (reduce + (map #(get-calibration-number % digits) data))))

(defn part-2 []
  (let [digits (keys numbers)]
    (reduce + (map #(get-calibration-number % digits) data))))