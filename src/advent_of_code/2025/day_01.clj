(ns advent-of-code.2025.day-01
  (:require [advent-of-code.shared.read-file :as read]))

(def data (->> ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"]
               (map #(hash-map :direction (first %) :turn (Integer/parseInt (apply str (rest %)))))))

(def real-data (->> (read/read-file "resources/day_01.txt")
                    (map #(hash-map :direction (first %) :turn (Integer/parseInt (apply str (rest %)))))))

(defn part-1 []
  (loop [d real-data
         pointer 50
         cnt 0]
    (if (empty? d)
      {:pointer pointer :cnt cnt}
      (let [direction-fn (if (= \L (:direction (first d))) - +)
            new-pointer (mod (direction-fn pointer (:turn (first d))) 100)]
        (recur (rest d) new-pointer (if (zero? new-pointer) (inc cnt) cnt))))))

(defn get-next [pointer instruction]
  (let [direction-fn (if (= \L (:direction instruction)) - +)]
    (loop [p pointer
           cnt (:turn instruction)
           zeros 0]
      (if (< 100 cnt)
        (recur p (- cnt 100) (inc zeros))
        (let [with-dir (direction-fn p cnt)
              next-pointer (mod with-dir 100)]
          {:pointer next-pointer :zeros (cond
                                          (or (zero? with-dir) (and (not (zero? p))
                                                                    (neg-int? with-dir)))
                                          (inc zeros)

                                          (<= 100 with-dir)
                                          (inc zeros)

                                          :else
                                          zeros)})))))

(defn part-2 []
  (loop [d real-data
         pointer 50
         cnt 0]
    (if (empty? d)
      {:pointer pointer :cnt cnt}
      (let [next-vals (get-next pointer (first d))]
        (recur (rest d) (:pointer next-vals) (+ cnt (:zeros next-vals)))))))