(ns advent-of-code.2021.day-8
  (:require [advent-of-code-2021.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def data (read/read-file "resources/day_08.txt"))
(def test-data (read/read-file "resources/day_08_p.txt"))

(defn format-line [line]
  (let [[x y z] (partition-by #(= % "|") (str/split line #" | "))]
    [x z]))

(defn find-specific-digits-counts [[_ output]]
  (count (filter #(contains? #{2 3 4 7} (count %)) output)))

(defn part-1 [input]
    (reduce + (map #(find-specific-digits-counts (format-line %)) input)))

;; Please don't look a this too carefully
;; This code is trash
(defn find-digits [[line _]]
  (let [find-input (fn [l n] (first (filter #(= (count %) n) l)))
        filter-by-count (fn [l cnt] (filter #(= (count %) cnt) l))
        ;; Initialize findings with character counts we already know
        findings {8 (find-input line 7)
                  7 (find-input line 3)
                  4 (find-input line 4)
                  1 (find-input line 2)}
        ;; combine 7 and 4, find which string is only 1 character away from combination (9)
        findings (assoc findings 9 (first (filter
                                            #(= (count (set/difference (set %)
                                                                       (set/union (set (get findings 7)) 
                                                                                  (set (get findings 4)))))
                                                1)
                                            (filter-by-count line 6))))
        ;; A five count number that is one different from 9
        findings (assoc findings 2 (first (filter #(= (count (set/difference (set (get findings 9)) (set %))) 2) 
                                                  (filter-by-count line 5))))
        ;; A five count number that wasn't already found
        threes-or-5s (set (map set (filter #(not (contains? (set (vals findings)) %)) (filter-by-count line 5))))
        ;; A five count number that has all the characters from 1
        threes (first (filter #(empty? (set/difference (set (get findings 1)) %)) threes-or-5s))
        ;; A five count number that doesn't have all characters from 1
        fives (first (filter #(not (empty? (set/difference (set (get findings 1)) %))) threes-or-5s))
        ;; Add 3 to findings
        findings (assoc findings 3 (first (filter #(= (set %) threes) (filter-by-count line 5))))
        ;; Add 5 to findings
        findings (assoc findings 5 (first (filter #(= (set %) fives) (filter-by-count line 5))))
        ;; A six count number that wasn't already found
        zero-or-six (set (map set (filter #(not (contains? (set (map set (vals findings))) (set %))) (filter-by-count line 6))))
        ;; A six count number that has all the characters from 1
        zero (first (filter #(empty? (set/difference (set (get findings 1)) %)) zero-or-six))
        ;; A six count number that doesn't have all the characters from 1
        six (first (filter #(not (empty? (set/difference (set (get findings 1)) %))) zero-or-six))
        ;; Add 0 to findings
        findings (assoc findings 0 (first (filter #(= (set %) zero) (filter-by-count line 6))))
        ;; Add 6 to findings
        findings (assoc findings 6 (first (filter #(= (set %) six) (filter-by-count line 6))))]
    findings))
       
(defn find-number-from-string [output-str findings]
  (ffirst (filter #(= (set (val %)) (set output-str)) findings)))

(defn get-output-number [output findings]
    (map #(find-number-from-string % findings) (map set output)))

(defn get-output-for-line [input]
  (let [[line output] (format-line input)
        findings (find-digits [line output])]
    (get-output-number output findings)))

(defn part-2 [input] ;; 1009098
  (reduce + (map #(Integer/parseInt (apply str %)) 
                 (map get-output-for-line input))))

