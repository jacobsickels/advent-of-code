(ns advent-of-code.2024.day-24
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_24.txt")
               (partition-by empty?)
               (remove #(= "" (first %)))))

(def wires (->> (first data)
                (reduce (fn [acc s]
                          (let [[n v] (str/split s #": ")]
                            (assoc acc n (Integer/parseInt v))))
                        {})))

(def gates (->> (second data)
                (map #(let [[gate output] (str/split % #" -> ")
                            [a type b] (str/split gate #" ")]
                        { :output output :a a :b b :type type}))))


(defn AND [a b] (if (= 1 a b) 1 0))
(defn OR [a b] (if (or (= 1 a) (= 1 b)) 1 0))
(defn XOR [a b] (if (= #{1 0} (conj #{} a b)) 1 0))


(defn do-gate [gate output-wires]
  (let [a (get output-wires (:a gate))
        b (get output-wires (:b gate))
        type (:type gate)
        output (:output gate)]
    (if (or (nil? a) (nil? b))
      output-wires ;; can't do this gate yet
      (cond
        (= type "AND")
        (assoc output-wires output (AND a b))

        (= type "OR")
        (assoc output-wires output (OR a b))

        (= type "XOR")
        (assoc output-wires output (XOR a b))))))

(defn part-1 [gates]
  (loop [g gates
         w (merge wires (reduce (fn [acc g] (assoc acc (:output g) nil)) {} gates))]
    (let [z-outputs (->> (filter (fn [[k v]] (str/starts-with? k "z")) w))]
      (if (not (contains? (set (map last z-outputs)) nil))
        (Long/parseLong (->> (sort-by first z-outputs)
                             reverse
                             (map last)
                             (apply str)) 2)
        (if (empty? g)
          (recur gates w)
          (let [new-wires (do-gate (first g) w)]
            (recur (rest g) new-wires)))))))

(defn return-outputs [gates]
  (loop [g gates
         w (merge wires (reduce (fn [acc g] (assoc acc (:output g) nil)) {} gates))]
    (let [z-outputs (->> (filter (fn [[k v]] (str/starts-with? k "z")) w))]
      (if (not (contains? (set (map last z-outputs)) nil))
        (->> (sort-by first z-outputs)
             reverse
             (map last)
             (apply str))
        (if (empty? g)
          (recur gates w)
          (let [new-wires (do-gate (first g) w)]
            (recur (rest g) new-wires)))))))

;(defn part-2 []
;  (let [x (Long/parseLong (->> (filter (fn [[k v]] (str/starts-with? k "x")) wires)
;                               (sort-by first)
;                               reverse
;                               (map last)
;                               (apply str)) 2)
;        y (Long/parseLong (->> (filter (fn [[k v]] (str/starts-with? k "y")) wires)
;                               (sort-by first)
;                               reverse
;                               (map last)
;                               (apply str)) 2)]
;    (- (part-1 gates) (+ x y)
;       (Long/parseLong (str "1" (apply str (repeat 33 "0"))) 2)
;       (Long/parseLong (str "1" (apply str (repeat 25 "0"))) 2)
;       (Long/parseLong (str "1" (apply str (repeat 21 "0"))) 2))))

(defn swap-outputs [gates output-1 output-2]
  (let [gate-1 (first (filter #(= (:output %) output-1) gates))
        gate-2 (first (filter #(= (:output %) output-2) gates))]
    (conj (filter #(or (= (:output %) output-1) (= (:output %) output-2)) gates)
          (assoc gate-1 :output output-2)
          (assoc gate-2 :output output-1))))

(defn part-2 []
  (part-1 (swap-outputs gates "z21" "z22")))