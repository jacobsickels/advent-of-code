(ns advent-of-code.2022.day-10
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2022/day_10.txt")
               (map #(str/split % #" "))
               (map #(if (str/starts-with? (first %) "add")
                       [(first %) (Integer/parseInt (second %)) 1]
                       (conj % 0)))))

(defn dec-last [operation]
  (conj (vec (butlast operation)) (dec (last operation))))

(def check-cycles #{20 60 100 140 180 220})

(defn execute []
  (loop [operations data
         x 1
         cycles 1
         values []]
    (if (empty? operations)
      values
      (let [[op a b] (first operations)]
        (cond
          (and (= op "addx") (zero? b))
          (recur (rest operations)
                 (+ x a)
                 (inc cycles)
                 (if (contains? check-cycles cycles) (conj values (* cycles x)) values))

          (and (= op "noop") (zero? a))
          (recur (rest operations)
                 x
                 (inc cycles)
                 (if (contains? check-cycles cycles) (conj values (* cycles x)) values))

          :else (recur (conj (rest operations) (dec-last (first operations)))
                       x
                       (inc cycles)
                       (if (contains? check-cycles cycles) (conj values (* cycles x)) values)))))))

(defn part-1 [] (reduce + (execute)))

(defn should-add-pixel? [x crt-pos]
  (<= (dec x) crt-pos (inc x)))

(def crt-cycles #{40 80 120 160 200 240})

(defn execute-crt []
  (loop [operations data
         x 1
         cycles 1
         crt-pos 0
         pixels []]
    (if (empty? operations)
      pixels
      (let [[op a b] (first operations)]
        (cond
          (and (= op "addx") (zero? b))
          (recur (rest operations)
                 (+ x a)
                 (inc cycles)
                 (if (contains? crt-cycles cycles) 0 (inc crt-pos))
                 (conj pixels (if (should-add-pixel? x crt-pos) "#" ".")))

          (and (= op "noop") (zero? a))
          (recur (rest operations)
                 x
                 (inc cycles)
                 (if (contains? crt-cycles cycles) 0 (inc crt-pos))
                 (conj pixels (if (should-add-pixel? x crt-pos) "#" ".")))

          :else (recur (conj (rest operations) (dec-last (first operations)))
                       x
                       (inc cycles)
                       (if (contains? crt-cycles cycles) 0 (inc crt-pos))
                       (conj pixels (if (should-add-pixel? x crt-pos) "#" "."))))))))

(defn part-2 [] (->> (execute-crt)
                     (partition 40)
                     (map #(apply str %))))