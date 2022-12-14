(ns advent-of-code.2021.day-6
  (:require [advent-of-code-2021.core :as core]))

(def data [5, 1, 1, 3, 1, 1, 5, 1, 2, 1, 5, 2, 5, 1, 1, 1, 4, 1, 1, 5, 1, 1, 4, 1, 1, 1, 3, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 5, 1, 1, 1, 4, 1, 1, 1, 1, 1, 3, 1, 1, 4, 1, 4, 1, 1, 2, 3, 1, 1, 1, 1, 4, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 4, 4, 1, 4, 2, 1, 1, 1, 1, 1, 4, 3, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 3, 1, 2, 1, 1, 4, 1, 1, 5, 3, 1, 1, 1, 2, 4, 1, 1, 2, 4, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 4, 3, 1, 2, 1, 2, 1, 5, 1, 2, 1, 1, 5, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 3, 1, 1, 5, 1, 1, 1, 1, 5, 1, 4, 1, 1, 1, 4, 1, 3, 4, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 5, 1, 3, 1, 1, 1, 1, 4, 1, 5, 3, 1, 1, 1, 1, 1, 5, 1, 1, 1, 2, 2])

(def test-data [3, 4, 3, 1, 2])

(defn dec-cycle [input]
  (let [num-zeros (get input 0)]
    (reduce
      (fn [acc [k v]] (assoc acc k (cond (= k 8)
                                         num-zeros

                                         (= k 6)
                                         (+ (get input 7) (get input 0))

                                         :else
                                         (get input (inc k)))))
      {}
      (reverse (sort input)))))

(defn count-fish [input num-days]
  (loop [d   (merge
               {0 0
                1 0
                2 0
                3 0
                4 0
                5 0
                6 0
                7 0
                8 0}
               (frequencies input))
         day 0]
    (if (= day num-days)
      (reduce + (vals d))
      (recur (dec-cycle d) (inc day)))))

(defn part-1 [input]
  (count-fish input 80))

(defn part-2 [input]
  (count-fish input 256))