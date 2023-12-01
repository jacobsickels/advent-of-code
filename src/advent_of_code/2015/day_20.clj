(ns advent-of-code.2015.day-20)

(def prime-numbers
  ((fn f [x]
     (cons x
           (lazy-seq
             (f (first
                  (drop-while
                    (fn [n]
                      (some #(zero? (mod n %))
                            (take-while #(<= (* % %) n) prime-numbers)))
                    (iterate inc (inc x))))))))
   2))

(defn factorize [n]
  ((fn f [n [h & r :as ps]]
     (cond (< n 2) '()
           (zero? (mod n h)) (cons h (lazy-seq (f (quot n h) ps)))
           :else (recur n r)))
   n prime-numbers))

(defn num-presents [n]
  (->> (set (conj (factorize n) 1 n))
       (reduce +)))

(defn part-1 []
  (loop [check-num 1] ;; 2703201
    (let [presents (num-presents check-num)]
      (println check-num presents)
      (if (<= 3600000 presents)
        check-num
        (recur (inc check-num))))))