(ns advent-of-code.2021.day-10
  (:require [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

(def data (read/read-file "resources/day_10.txt"))
(def test-data (read/load-edn "day_10.edn"))

(def left #{\( \[ \{ \<})
(def right #{\) \] \} \>})
(def pairs #{[\{ \}]
             [\[ \]]
             [\< \>]
             [\( \)]})

(defn is-matching? [left right]
  (contains? pairs [left right]))

(defn find-stack-type [line]
  (loop [l     line
         stack []]
    (if (empty? l)
      [:incomplete stack line]
      (let [next (first l)]
        (cond (contains? left next)
              (recur (rest l) (cons next stack))

              (contains? right next)
              (if (is-matching? (first stack) next)
                (recur (rest l) (drop 1 stack))
                [:corrupted (first stack) next line]))))))

(defn part-1 []
  (->> data
       (map find-stack-type)
       (filter #(= (first %) :corrupted))
       (map #(nth % 2))
       (replace {\) 3 \] 57 \} 1197 \> 25137})
       (reduce +)))

(defn get-finishing-char [left]
  (cond
    (= left \() \)
    (= left \[) \]
    (= left \{) \}
    (= left \<) \>))

(defn get-score-for-line [line]
  (reduce #(+ %2 (* %1 5))
          0
          (replace {\) 1 \] 2 \} 3 \> 4} line)))

(defn get-middle [col]
  (nth col (quot (count col) 2)))

(defn part-2 []
  (->> data
       (map find-stack-type)
       (filter #(= (first %) :incomplete))
       (map #(nth % 1))
       (map #(map get-finishing-char %))
       (map get-score-for-line)
       sort
       get-middle))
