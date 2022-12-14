(ns advent-of-code.2021.day-2
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def data (read/read-file "resources/day_02.txt"))


(def test-data ["forward 5"
                "down 5"
                "forward 8"
                "up 3"
                "down 8"
                "forward 2"])

(defn part-1 [input]
  (loop [x 0
         y 0
         acc input]
    (if (empty? acc)
      (* x y)
      (let [change (Integer/parseInt (str (last (first acc))))]
        (cond
          (str/starts-with? (first acc) "forward")
          (recur (+ x change) y (rest acc))
      
          (str/starts-with? (first acc) "down")
          (recur x (+ y change) (rest acc))

          (str/starts-with? (first acc) "up")
          (recur x (- y change) (rest acc)))))))

(defn part-2 [input]
  (loop [x 0
         y 0
         aim 0
         acc input]
    (if (empty? acc)
      (* x y)
      (let [change (Integer/parseInt (str (last (first acc))))]
        (cond
          (str/starts-with? (first acc) "forward")
          (recur (+ x change) 
                 (+ y (* aim change))
                 aim
                 (rest acc))

          (str/starts-with? (first acc) "down")
          (recur x y (+ aim change) (rest acc))

          (str/starts-with? (first acc) "up")
          (recur x y (- aim change) (rest acc)))))))