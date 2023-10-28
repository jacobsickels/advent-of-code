(ns advent-of-code.2022.day-25
  (:require [advent-of-code.shared.read-file :as read]))

(def data (read/read-file "resources/2022/day_25.txt"))

(defn radix-5 [numbers]
  (reduce (fn [acc n] (+ (* acc 5) n)) 0 numbers))

(defn place-values
  ([] (place-values 1 0))
  ([n i] (lazy-seq (cons n (place-values (Math/pow 5 (inc i)) (inc i))))))


(defn place-values-for-number [number]
  (take-while #(>= (+ number 5) %) (place-values)))

(def snafu-to-decimal {"2" 2 "1" 1 "0" 0 "-" -1 "=" -1})
(def decimal-to-snafu {2 "2" 1 "1" 0 "0" 4 "-" 3 "="})

(defn make-snafu
  [number]
  (loop [numb number
         places 1
         acc []]
    (println numb places)
    (if (<= numb places)
      (apply str (reverse acc))
      (let [divided (quot numb places)
            remainder (mod divided 5)]
        (recur
          (cond
            (= 3 remainder) (+ numb (* 2  places))
            (= 4 remainder) (+ numb places)
            :else numb)
          (* 5 places)
          (conj acc (get decimal-to-snafu remainder)))))))

(defn parse [line]
  (map #(get snafu-to-decimal (str %)) line))

(defn part-1 []
  (->> (map parse data)
       (map radix-5)
       (reduce +)
       (make-snafu)))