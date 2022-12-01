(ns advent-of-code.2015.day-15
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn has-100-teaspons? [col]
  (= 100 (reduce + col)))

(defn get-valid-checks []
  (->> (combo/cartesian-product (range 1 98)
                                (range 1 98)
                                (range 1 98)
                                (range 1 98))
       (filter has-100-teaspons?)))

(def capacity [5 -1 0 -1])
(def durability [-1 3 -1 0])
(def flavor [0 0 4 0])
(def texture [0 0 0 2])

(defn score-for-type [type-col col]
  (let [score (reduce + (map (fn [x y] (* x y))
                             type-col
                             col))]
    (if (> score 0) score 0)))

(defn get-score [col]
  (* (score-for-type capacity col)
     (score-for-type durability col)
     (score-for-type flavor col)
     (score-for-type texture col)))


