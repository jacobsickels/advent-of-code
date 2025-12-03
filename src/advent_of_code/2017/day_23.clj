(ns advent-of-code.2017.day-23
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def instructions (->> (read/read-file "resources/2017/day_23.txt")
                       (map #(str/split % #" "))))

(defn is-number? [s]
  (if (str/starts-with? s "-")
    (every? #(Character/isDigit %) (rest s))
    (every? #(Character/isDigit %) s)))

(defn safe-get [registers y]
  (if (is-number? y) (Integer/parseInt y) (get registers (keyword y))))

(defn sset [registers x y pointer]
  [(assoc registers (keyword x) (safe-get registers y)) (+ pointer 1)])

(defn sub [registers x y pointer]
  [(assoc registers (keyword x) (- (safe-get registers x) (safe-get registers y))) (+ pointer 1)])

(defn mul [registers x y pointer]
  [(assoc registers (keyword x) (* (safe-get registers x) (safe-get registers y))) (+ pointer 1)])

(defn jnz [registers x y pointer]
  (if (zero? (safe-get registers x))
    [registers (+ pointer 1)]
    [registers (+ pointer (safe-get registers y))]))


(defn do-instruction [registers [instruction a b] pointer]
  (cond
    (= instruction "set") (sset registers a b pointer)
    (= instruction "sub") (sub registers a b pointer)
    (= instruction "mul") (mul registers a b pointer)
    (= instruction "jnz") (jnz registers a b pointer)
    :else nil))


(defn part-1 []
  (loop [registers {:a 0 :b 0 :c 0 :d 0 :e 0 :f 0 :g 0 :h 0}
         pointer 0
         mul-cnt 0]
    (if (>= pointer (count instructions))
      [registers mul-cnt]
      (let [instruction (nth instructions pointer)
            [next-registers next-pointer] (do-instruction registers instruction pointer)]
        (if (= "mul" (first instruction))
          (recur next-registers next-pointer (inc mul-cnt))
          (recur next-registers next-pointer mul-cnt))))))

(defn run-program []
  (loop [registers {:a 1 :b 0 :c 0 :d 0 :e 0 :f 0 :g 0 :h 0}
         pointer 0]
    (println registers)
    (if (>= pointer (count instructions))
      registers
      (let [instruction (nth instructions pointer)
            [next-registers next-pointer] (do-instruction registers instruction pointer)]
        (recur next-registers next-pointer)))))