(ns advent-of-code.2017.day-08
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2017/day_08.txt")))

(defn is-check-true? [[in-reg func n] registers]
  (let [parsed-n (Integer/parseInt n)
        check-reg (get registers in-reg 0)]
    (cond
      (= func "<")  (< check-reg parsed-n)
      (= func ">")  (> check-reg parsed-n)
      (= func "<=") (<= check-reg parsed-n)
      (= func ">=") (>= check-reg parsed-n)
      (= func "==") (= check-reg parsed-n)
      (= func "!=") (not= check-reg parsed-n))))

(defn modify-registers [[to-change func n] registers]
  (let [parsed-n (Integer/parseInt n)
        current-val (get registers to-change 0)]
    (assoc registers to-change ((if (= func "inc") + -) current-val parsed-n))))

(defn do-instruction [instruction registers]
  (let [[to-change func n _ & check] (str/split instruction #" ")]
    (if (is-check-true? check registers)
      (modify-registers [to-change func n] registers)
      registers)))

(defn part-1 []
  (loop [registers {}
         instructions data]
    (if (empty? instructions)
      (apply max (vals registers))
      (recur (do-instruction (first instructions) registers) (rest instructions)))))

(defn part-2 []
  (loop [registers {}
         instructions data
         highest-values []]
    (if (empty? instructions)
      (apply max highest-values)
      (let [new-registers (do-instruction (first instructions) registers)]
        (recur new-registers (rest instructions) (conj highest-values (apply max (vals new-registers))))))))