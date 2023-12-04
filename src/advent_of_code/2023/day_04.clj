(ns advent-of-code.2023.day-04
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [line]
  (let [[card numbers] (str/split line #": ")
        [winning have] (map str/trim (str/split numbers #"\|"))]
    {:id (->> (str/split card #" ")
              (map str/trim)
              (remove empty?)
              second
              Integer/parseInt)
     :winning  (->> (str/split winning #" ")
                    (remove empty?)
                    (map #(Integer/parseInt %))
                    set)
     :have (->> (str/split have #" ")
                (remove empty?)
                (map #(Integer/parseInt %))
                set)
     :copies 1}))

(def cards (->> (read/read-file "resources/2023/day_04.txt")
                (map parse)))

(defn get-num-wins [card]
  (let [num-winning (count (set/intersection (:winning card) (:have card)))]
    (int (Math/pow 2 (dec num-winning)))))

(defn part-1 []
  (reduce + (map get-num-wins cards)))

(defn get-card [cards card-id]
  (first (filter #(= (:id %) card-id) cards)))

(defn add-wins [cards card-id]
  (let [card (get-card cards card-id)
        wins (:wins card)
        copies (:copies card)
        to-add (set (range (inc card-id) (+ card-id wins 1)))]
    (reduce (fn [acc c-card]
              (if (contains? to-add (:id c-card))
                (conj acc (assoc c-card :copies (+ (:copies c-card) copies)))
                (conj acc c-card)))
            [] cards)))

(defn part-2 []
  (let [cards-with-copies (reduce (fn [acc card]
                                    (conj acc (assoc card :wins (count (set/intersection (:winning card) (:have card))))))
                                  [] cards)]
    (->> (reduce (fn [acc card] (add-wins acc (:id card))) cards-with-copies cards-with-copies)
         (map :copies)
         (reduce +))))


