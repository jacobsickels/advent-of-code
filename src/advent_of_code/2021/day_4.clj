(ns advent-of-code.2021.day-4
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code.shared.read-file :as read]))

(def data (read/read-file "resources/day_04.txt"))

(def test-data ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
                ""
                "22 13 17 11  0"
                "8  2 23  4 24"
                "21  9 14 16  7"
                "6 10  3 18  5"
                "1 12 20 15 19"
                ""
                "3 15  0  2 22"
                "9 18 13 17  5"
                "19  8  7 25 23"
                "20 11 10 24  4"
                "14 21 16 12  6"
                ""
                "14 21 17 24  4"
                "10 16 15  9 19"
                "18  8 23 26 20"
                "22 11 13  6  5"
                "2  0 12  3  7"])

(defn line-to-numbers [line]
  (map #(Integer/parseInt %) (remove empty? (str/split line #" "))))

(defn format-data [input]
  (let [[numbers & boards] (remove #(= (first %) "") (partition-by empty? input))]
    {:numbers (map #(Integer/parseInt %) (str/split (first numbers) #","))
     :boards (map #(map line-to-numbers %) boards)}))

(defn board-is-winner? [board selected]
  (if (< (count selected) 5)
    false
    (let [rows-check (map #(set/difference % selected) (map set board))
          row-winner? (some empty? rows-check)
          columns (map #(map (fn [x] (nth x %)) board) (range 0 5))
          columns-check (map #(set/difference (set %) selected) columns)
          column-winner? (some empty? columns-check)]
      (or row-winner? column-winner? false))))

(defn get-winning-board [boards selected]
  (filter #(board-is-winner? % selected) boards))

(defn play-game [numbers boards selected]
  (loop [ns numbers
         s selected
         last-selected nil]
    (if (empty? ns)
      :no-winner
      (let [winning-board (get-winning-board boards s)]
        (if (empty? winning-board)
          (recur (rest ns) (conj s (first ns)) (first ns))
          [ns (first winning-board) s last-selected])))))

(defn part-1 [input]
  (let [formatted (format-data input)
        numbers (:numbers formatted)
        boards  (:boards formatted)
        [_ winning-board selected last-number-called] (play-game numbers boards #{})]
    (* last-number-called (reduce + (set/difference (set (flatten winning-board)) selected)))))

(defn part-2 [input]
  (let [formatted (format-data input)]
    (loop [boards (:boards formatted)
           numbers (:numbers formatted)
           last-board nil
           selected #{}
           last-number-called nil]
      (if (empty? boards)
        (* (reduce + (set/difference (set (flatten last-board)) selected)) last-number-called)
        (let [[remaining-numbers winning-board selected last-number-called] (play-game numbers boards selected)]
          (recur 
            (remove #(= winning-board %) boards)
            remaining-numbers
            winning-board
            selected
            last-number-called))))))
        