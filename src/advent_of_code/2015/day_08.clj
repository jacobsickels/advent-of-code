(ns advent-of-code.2015.day-08
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- count-character-literals [col]
  (count col))

(defn unhexify [hex]
  (apply str
         (map
           (fn [[x y]] (char (Integer/parseInt (str x y) 16)))
                (partition 2 hex))))

(defn- count-memory-characters [col]
  (str/escape col #{\"}))

(defn part-1 []
  (let [data (read/read-file "resources/2015/day_08_test.txt")]
    (map #(vector (count-character-literals %) (str/trim %)) data)))
