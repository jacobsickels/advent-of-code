(ns advent-of-code.2022.day-20-1
  (:require [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "2022/day_20.edn"))

(defn make-circular-linked-list [col]
  (first (reduce
           (fn [[acc l] item] [(assoc acc l item) item])
           [{} (last col)]
           col)))

(defn get-mod [col-size to-move]
  (mod (second to-move) (dec col-size)))

(defn get-number-away [linked-list to-move]
  (loop [amount (if (pos? (second to-move))
                  (mod (second to-move) (dec (count linked-list)))
                  (mod (second to-move) (count linked-list)))
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
  (if (zero? (second to-move))
    linked-list
    (let [location (get-number-away linked-list to-move)]
      (move-number linked-list to-move location))))

(defn do-mix [col]
  (let [indexed-col (map-indexed (fn [i n] (list i n)) col)]
    (loop [idx-col indexed-col
           linked-list (make-circular-linked-list indexed-col)]
      (if (empty? idx-col)
        linked-list
        (recur (rest idx-col) (mix linked-list (first idx-col)))))))


(defn get-number-amt-ahead [circle start amt-ahead]
  (loop [amount amt-ahead
         found start]
    (cond
      (zero? amount) found

      :else (recur (dec amount) (get circle found)))))

(defn part-1 [col]
  (let [circle (do-mix col)
        start (->> (map-indexed (fn [i n] (list i n)) col)
                   (filter #(zero? (second %)))
                   (first))
        _ (println start)
        f (get-number-amt-ahead circle start 1000)
        s (get-number-amt-ahead circle start 2000)
        t (get-number-amt-ahead circle start 3000)]
    [circle (->> (map second [f s t])
                 (reduce +))]))

