(ns advent-of-code.2019.day-05
  (:require [clojure.string :as str]))

(defn digits
  [number]
  (map #(Integer/parseInt %) (str/split (str number) #"")))

(defn get-file-contents []
  (map #(Integer/parseInt %)
       (str/split (first
                    (with-open [reader (clojure.java.io/reader "resources/day_5_input.txt")]
                      (reduce conj [] (line-seq reader)))) #",")))

(defn get-op-code [num]
  (if (<= num 8)
    num
    (rem num 100)))

(defn get-modes [num]
  (cond
    (< num 3) (take 3 (repeat 0))
    (< num 5) (list 0)
    (< num 7) (list 0 0)
    (< num 9) (take 3 (repeat 0))
    (< num 1000) (reverse (conj (digits (int (/ num 100))) 0 0))
    :else (reverse (conj (digits (int (/ num 100))) 0))))

(defn get-next-instruction [input opcode pos]
  (cond
    (or (= opcode 5) (= opcode 6))
    (take 3 (nthrest input pos))

    (or (= opcode 1) (= opcode 2) (= opcode 7) (= opcode 8))
    (take 4 (nthrest input pos))

    (or (= opcode 3) (= opcode 4))
    (take 2 (nthrest input pos))))

(defn parse-mode [input arg mode]
  (if (= mode 0)
    (get (vec input) arg)
    arg))

(defn make-instructions [input]
  (loop [codes input
         pos 0]
    (let [instruction (get-next-instruction codes (get-op-code (get codes pos)) pos)]
      (if (not (nil? instruction))
        (let [opcode (get-op-code (first instruction))
              args (rest instruction)
              modes (get-modes (first instruction))
              mode-vals (map #(parse-mode codes %1 %2) args modes)]
          (case opcode
            1 (recur (assoc (vec codes) (last args) (+ (first mode-vals) (second mode-vals))) (+ pos 4))

            2 (recur (assoc (vec codes) (last args) (* (first mode-vals) (second mode-vals))) (+ pos 4))

            3 (recur (assoc (vec codes) (first args) (let [input (read-line)] (Integer/parseInt input))) (+ pos 2))

            4 (do (println "OUT" (get (vec codes) (first args)))
                  (recur (vec codes) (+ pos 2)))

            5 (recur (vec codes) (if (not (= (first mode-vals) 0)) (second mode-vals) (+ pos 3)))

            6 (recur (vec codes) (if (= (first mode-vals) 0) (second mode-vals) (+ pos 3)))

            7 (recur (assoc (vec codes) (last args) (if (< (first mode-vals) (second mode-vals)) 1 0)) (+ pos 4))

            8 (recur (assoc (vec codes) (last args) (if (= (first mode-vals) (second mode-vals)) 1 0)) (+ pos 4))))))))