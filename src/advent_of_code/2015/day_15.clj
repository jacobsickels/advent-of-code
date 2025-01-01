(ns advent-of-code.2015.day-15)

(def capacity [5 -1 0 -1])
(def durability [-1 3 -1 0])
(def flavor [0 0 4 0])
(def texture [0 0 0 2])

(def calories [5 1 6 8])


(def capacity-test [-1 2])
(def durability-test [-2 3])
(def flavor-test [6 -2])
(def texture-test [3 -1])


(defn score-for-type [type-col col]
  (let [score (reduce + (map (fn [x y] (* x y))
                             type-col
                             col))]
    (if (> score 0) score 0)))

(defn get-score [col]
  (* (score-for-type capacity col)
     (score-for-type durability col)
     (score-for-type flavor col)
     (score-for-type texture col)))

(defn get-score-test [col]
  (* (score-for-type capacity-test col)
     (score-for-type durability-test col)
     (score-for-type flavor-test col)
     (score-for-type texture-test col)))

(defn redistribute-point [col n]
  (->> (range (count col))
       (remove #(= n %))
       (map #(update col % inc))
       (map (fn [c] (update c n dec)))
       (map #(vector % (get-score %)))
       (sort-by last)
       last))


(defn part-1 [col]
  (loop [points col
         last-score (get-score col)
         idx 0]
    (if (= idx (count col))
      [points last-score]
      (let [[new-points score] (redistribute-point points idx)]
        (if (> score last-score)
          (recur new-points score idx)
          (recur points last-score (inc idx)))))))


;(defn redistribute-point-for-calories [col n]
;  (->> (range (count col))
;       (remove #(= n %))
;       (map #(update col % inc))
;       (map (fn [c] (update c n dec)))
;       (map #(vector % (get-score %) (score-for-type calories %)))
;       (filter #(<= (last %) 500))
;       (sort-by last)
;       (last)))
;
;(defn part-2 [col]
;  (loop [points col
;         last-score (get-score col)
;         idx 0]
;    (if (= idx (count col))
;      [points last-score]
;      (let [[new-points score calories] (redistribute-point-for-calories points idx)]
;        (cond
;          (nil? new-points)
;          (recur points last-score (inc idx))
;
;          (= calories 500)
;          (recur new-points score (inc idx))
;
;          (> score last-score)
;          (recur new-points score idx)
;
;          :else
;          (recur points last-score (inc idx)))))))


