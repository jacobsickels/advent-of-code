(ns advent-of-code.2023.day-07
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2023/day_07.txt")
               (map #(let [[hand bet] (str/split % #" ")]
                       [(str/split hand #"") (Integer/parseInt bet)]))))

(def ranks ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])

(def type-rank [:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind :two-pair :one-pair :high-card])

(defn get-hand-type [hand]
  (let [freq (frequencies hand)]
    (cond
      (= 5 (first (sort (vals freq))))
      :five-of-a-kind

      (= [1 4] (sort (vals freq)))
      :four-of-a-kind

      (= [2 3] (sort (vals freq)))
      :full-house

      (= [1 1 3] (sort (vals freq)))
      :three-of-a-kind

      (= [1 2 2] (sort (vals freq)))
      :two-pair

      (= [1 1 1 2] (sort (vals freq)))
      :one-pair

      (= [1 1 1 1 1] (sort (vals freq)))
      :high-card)))

(defn parse-hand-types [hand-type-fn rank-vec]
  (map (fn [[hand bet]] [hand
                         bet
                         (hand-type-fn hand)
                         (mapv #(first (index-of % (reverse rank-vec))) hand)])
       data))

(defn index-of [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn order-hands [parsed-ranks]
  (->> (sort-by #(first (index-of (nth % 2) type-rank)) parsed-ranks)
       (partition-by #(nth % 2))
       (map (fn [part] (sort (comp + compare) (map last part))))
       reverse
       (apply concat)))

(defn part-1 []
  (let [parsed-ranks (parse-hand-types get-hand-type ranks)
        hand-order (order-hands parsed-ranks)]
    (->> (map #(conj % (inc (first (index-of (last %) hand-order)))) (parse-hand-types get-hand-type ranks))
         (reduce (fn [acc [_ winnings _ _ rank]] (+ acc (* winnings rank))) 0))))

(def ranks-2 ["A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2" "J"])

(defn find-j-should-be [hand]
  (let [hand-types (->> (sort-by val (frequencies hand))
                        (partition-by second))]
    (if (empty? (->> (last hand-types)
                     (remove #(= "J" (first %)))))
      (->> (drop-last hand-types)
           last
           last
           first)
      (->> (last hand-types)
           (remove #(= "J" (first %)))
           last
           first))))

(defn get-hand-type-2 [hand]
  (let [j-should-be (find-j-should-be hand)
        hand (if (contains? (set hand) "J")
               (replace {"J" j-should-be} hand)
               hand)]
    (get-hand-type hand)))

(defn part-2 []
  (let [parsed-ranks (parse-hand-types get-hand-type-2 ranks-2)
        hand-order (order-hands parsed-ranks)]
    (->> (map #(conj % (inc (first (index-of (last %) hand-order)))) (parse-hand-types get-hand-type-2 ranks-2))
         (reduce (fn [acc [_ winnings _ _ rank]] (+ acc (* winnings rank))) 0))))