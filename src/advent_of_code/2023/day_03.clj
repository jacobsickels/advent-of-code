(ns advent-of-code.2023.day-03
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.point :as points]
            [clojure.set :as set]))

(defn find-numbers [col row]
  (loop [check col
         parts []
         points []
         final []
         pointer 0]
    (cond
      (and (empty? check) (not-empty parts))
      (conj final [(Integer/parseInt (apply str parts)) points])

      (empty? check)
      final

      (Character/isDigit (first check))
      (recur (rest check) (conj parts (first check)) (conj points [pointer row]) final (inc pointer))

      (and (not (Character/isDigit (first check))) (not-empty parts))
      (recur (rest check) [] [] (conj final [(Integer/parseInt (apply str parts)) points]) (inc pointer))

      :else
      (recur (rest check) parts points final (inc pointer)))))


(defn find-symbols [col row]
  (loop [check col
         final []
         pointer 0]
    (if (empty? check)
      final
      (cond
        (and (not (Character/isDigit (first check))) (not= \. (first check)))
        (recur (rest check) (conj final [(first check) [pointer row]]) (inc pointer))

        :else
        (recur (rest check) final (inc pointer))))))


(def numbers (->> (read/read-file "resources/2023/day_03.txt")
                  (map-indexed (fn [i c] (find-numbers c i)))
                  (remove empty?)
                  (apply concat)))

(def symbols (->> (read/read-file "resources/2023/day_03.txt")
                  (map-indexed (fn [i c] (find-symbols c i)))
                  (remove empty?)
                  (apply concat)
                  (map second)
                  set))

(defn part-1 []
  (reduce + (loop [nums numbers
                   part-numbers []]
              (if (empty? nums)
                part-numbers
                (let [[check-num points] (first nums)
                      points-around (->> (map points/points-around points)
                                         (apply concat))]
                  (if (not-empty (set/intersection symbols (set points-around)))
                    (recur (rest nums) (conj part-numbers check-num))
                    (recur (rest nums) part-numbers)))))))

(def gears (->> (read/read-file "resources/2023/day_03.txt")
                (map-indexed (fn [i c] (find-symbols c i)))
                (remove empty?)
                (apply concat)
                (filter #(= \* (first %)))
                (map second)
                set))

(defn part-2 []
  (reduce + (loop [gs gears
                   part-numbers []]
              (if (empty? gs)
                part-numbers
                (let [gear (first gs)
                      points-around-gear (points/points-around gear)
                      adjacent-numbers (filter (fn [[_ number-points]]
                                                 (not-empty (set/intersection (set points-around-gear) (set number-points))))
                                               numbers)]
                  (if (= 2 (count adjacent-numbers))
                    (recur (rest gs) (conj part-numbers (reduce * (map first adjacent-numbers))))
                    (recur (rest gs) part-numbers)))))))