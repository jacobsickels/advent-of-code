(ns advent-of-code.2019.day-04
  (:require [clojure.string :as str]))


(defn digits
  [number]
  (map #(Integer/parseInt %) (str/split (str number) #"")))

(defn number-is-ordered
  [number]
  (apply <= (digits number)))

(defn number-has-duplicates
  [number]
  (not (apply distinct? (digits number))))

(defn number-has-double
  [number]
  (<= 1 (count (filter
                 (fn [[_ freq]] (= freq 2))
                 (frequencies (digits number))))))

(defn password
  []
  (count (let [min 109165
               max 576723]
           (filter #(and
                      (number-is-ordered %)
                      ;(number-has-duplicates %)
                      (number-has-double %))
                   (range min max)))))