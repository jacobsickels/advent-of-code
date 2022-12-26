(ns advent-of-code.2022.day-20
  (:require [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "2022/day_20.edn"))
(def test-data [1 2 -3 3 -2 0 4])

(defn make-circular-linked-list [col]
  (first (reduce
           (fn [[acc l] item] [(assoc acc l item) item])
           [{} (last col)]
           col)))

(defn get-mod [col-size to-move]
  (mod (second to-move) (dec col-size)))

(defn get-number-away [linked-list to-move]
  (loop [amount (mod (second to-move) (dec (count linked-list)))
         found to-move]
    (if (zero? amount)
      found
      (recur (dec amount) (get linked-list found)))))

(defn move-number [linked-list to-move location]
  (let [old-left (ffirst (filter (fn [[_ v]] (= v to-move)) linked-list))
        old-right (get linked-list to-move)
        location-right (get linked-list location)]
    (assoc linked-list old-left old-right
                       location to-move
                       to-move location-right)))

(defn mix [linked-list to-move]
  (if (or (zero? (second to-move)) (zero? (mod (second to-move) (dec (count linked-list)))))
    linked-list
    (let [location (get-number-away linked-list to-move)]
      (move-number linked-list to-move location))))

(defn do-mix [indexed-col starting-linked-list]
  (loop [idx-col indexed-col
         linked-list starting-linked-list]
    (if (empty? idx-col)
      linked-list
      (recur (rest idx-col) (mix linked-list (first idx-col))))))


(defn get-number-amt-ahead [circle start amt-ahead]
  (loop [amount amt-ahead
         found start]
    (cond
      (zero? amount) found

      :else (recur (dec amount) (get circle found)))))

(defn part-1 [col]
  (let [indexed-col (map-indexed (fn [i n] (list i n)) col)
        linked-list (make-circular-linked-list indexed-col)
        circle (do-mix indexed-col linked-list)
        start (->> (map-indexed (fn [i n] (list i n)) col)
                   (filter #(zero? (second %)))
                   (first))
        f (get-number-amt-ahead circle start 1000)
        s (get-number-amt-ahead circle start 2000)
        t (get-number-amt-ahead circle start 3000)]
    (->> (map second [f s t])
         (reduce +))))


(defn do-mix-10 [col]
  (let [indexed-col (map-indexed (fn [i n] (list i n)) col)
        linked-list (make-circular-linked-list indexed-col)]
    (loop [ll linked-list
           amount 10]
      (if (zero? amount)
        ll
        (recur (do-mix indexed-col ll)
               (dec amount))))))

(defn part-2 [col]
  (let [decrypted (map #(* % 811589153) col)
        circle (do-mix-10 decrypted)
        start (->> (map-indexed (fn [i n] (list i n)) col)
                   (filter #(zero? (second %)))
                   (first))
        f (get-number-amt-ahead circle start 1000)
        s (get-number-amt-ahead circle start 2000)
        t (get-number-amt-ahead circle start 3000)]
    (->> (map second [f s t])
         (reduce +))))

