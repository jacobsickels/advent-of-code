(ns advent-of-code.2018.day-07
  (:require [advent-of-code.shared.read-file :as read]))

(def test-data ["Step C must be finished before step A can begin."
                "Step C must be finished before step F can begin."
                "Step A must be finished before step B can begin."
                "Step A must be finished before step D can begin."
                "Step B must be finished before step E can begin."
                "Step D must be finished before step E can begin."
                "Step F must be finished before step E can begin."])

(def data (read/read-file "resources/2018/day_07.txt"))

(defn get-data [col]
  (map
    (fn [item]
      (let [pre (last (re-find #"Step \S" item))
            to (last (re-find #"step \S" item))]
        (vector pre to)))
    col))

(defn- has-dependencies? [c col]
  (seq? (seq (filter #(= (second %) c) col))))

(defn get-characters [col]
  (sort (set (flatten col))))

(defn- find-first-no-deps [current-chars col]
  (loop [characters current-chars]
    (if (not (has-dependencies? (first characters) col))
      (first characters)
      (recur (rest characters)))))

(defn remove-character-no-deps [character col]
  (remove #(= (first %) character) col))

(defn part-1 [col]
  (loop [data (get-data col)
         characters (get-characters data)
         found ""]
    (if (empty? characters)
      found
      (let [next-found (find-first-no-deps characters data)]
        (recur
          (remove-character-no-deps next-found data)
          (remove #(= % next-found) characters)
          (str found next-found))))))


; Maybe something like this
; ("C" "C" "C" "AF" "BF" "BF" "DF" "DF" "DF" "DF" "D" "E" "E" "E" "E" "E")
; count 0 workers {"C" 3}       list ()
; count 1 workers {"C" 2}       list ("C")
; count 2 workers {"C" 1}       list ("C" "C")
; count 3 workers {"A" 1 "F" 6} list ("C" "C" "C")
; count 4 workers {"B" 2 "F" 5} list ("C" "C" "C" "AF")
; count 5 workers {"B" 1 "F" 4} list ("C" "C" "C" "AF" "BF")

;; Will also have to store characters that have their work completed, can use this check to break