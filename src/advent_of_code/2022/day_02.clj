(ns advent-of-code.2022.day-02
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

;; \A Rock \B Paper \C Scissors
;; \X Rock \Y Paper \Z Scissors

(def data (->> (read/read-file "resources/2022/day_02.txt")
               (map #(str/split % #" "))))

(def janken-pon {"A" {"X" [:draw 1]
                      "Y" [:win 2]
                      "Z" [:lose 3]}
                 "B" {"X" [:lose 1]
                      "Y" [:draw 2]
                      "Z" [:win 3]}
                 "C" {"X" [:win 1]
                      "Y" [:lose 2]
                      "Z" [:draw 3]}})

(def outcome-score {:win 6
                    :draw 3
                    :lose 0})

(defn round-score [opponent self]
  (let [[outcome score] (get-in janken-pon [opponent self])]
    (+ score (get outcome-score outcome))))

(defn part-1 []
  (->> (map (fn [[o s]] (round-score o s)) data)
       (reduce +)))

;; \X lose \Y draw \Z win

(def janken-pon-2 {"A" {"X" "Z"
                        "Y" "X"
                        "Z" "Y"}
                   "B" {"X" "X"
                        "Y" "Y"
                        "Z" "Z"}
                   "C" {"X" "Y"
                        "Y" "Z"
                        "Z" "X"}})

(defn calculate-round [opponent win-type]
  (let [self (get-in janken-pon-2 [opponent win-type])
        [outcome score] (get-in janken-pon [opponent self])]
    (+ score (get outcome-score outcome))))

(defn part-2 []
  (->> (map (fn [[o s]] (calculate-round o s)) data)
       (reduce +)))
