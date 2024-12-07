(ns advent-of-code.2024.day-07
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_07.txt")
               (map #(re-seq #"\d+" %))
               (map (fn [col] (map #(BigInteger. %) col)))))


(defn pad-string [binary-string amount]
  (clojure.pprint/cl-format nil (str "~" amount ",'0d") binary-string))

(defn get-binary-numbers [number]
  (let [binary-numbers (map #(Integer/toBinaryString %) (range 0 number))
        max-length (count (last binary-numbers))]
    (map #(pad-string % max-length) binary-numbers)))

(defn get-function-combinations [amount-spaces]
  (let [binary-numbers (get-binary-numbers amount-spaces)]
    (map #(map (fn [c] (if (= \0 c) +' *')) %) binary-numbers)))

(defn apply-function-combination [operators function-combination]
  (loop [funcs function-combination
         ops (rest operators)
         acc (first operators)]
    (if (empty? ops)
      acc
      (recur (rest funcs) (rest ops) (apply (first funcs) [acc (first ops)])))))

(defn can-be-evaluated? [calculation]
  (let [[test-val & operators] calculation
        function-combinations (get-function-combinations (Math/pow 2 (dec (count operators))))]
    (->> (map #(apply-function-combination operators %) function-combinations)
         (some #(= test-val %)))))

(defn part-1 []
  (->> (map #(vector (can-be-evaluated? %) %) data)
       (filter #(true? (first %)))
       (map #(first (second %)))
       (reduce +)))
;; ========================

(defn get-trinary-numbers [number]
  (let [trinary-numbers (map #(Integer/toString % 3) (range 0 number))
        max-length (count (last trinary-numbers))]
    (map #(pad-string % max-length) trinary-numbers)))

(defn get-function-combinations-2 [amount-spaces]
  (let [binary-numbers (get-trinary-numbers amount-spaces)]
    (map #(map (fn [c] (cond
                         (= \0 c)
                         +'
                         (= \1 c)
                         *'

                         (= \2 c)
                         (fn [a b] (BigInteger. (str a b))))) %)
         binary-numbers)))

(defn can-be-evaluated-2? [calculation]
  (let [[test-val & operators] calculation
        function-combinations (get-function-combinations-2 (Math/pow 3 (dec (count operators))))]
    (->> (map #(apply-function-combination operators %) function-combinations)
         (some #(= test-val %)))))

(defn part-2 []
  (->> (map #(vector (can-be-evaluated-2? %) %) data)
       (filter #(true? (first %)))
       (map #(first (second %)))
       (reduce +)))
;; this is pretty slow, but Im also sortof brute forcing here
;; probably a way to cache some of the calls