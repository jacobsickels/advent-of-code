(ns advent-of-code.2019.day-01)

(defn fuel-req [input]
  (reduce + (map #(- (int (/ % 3)) 2) input)))

(defn recursive-fuel-req [input]
  (rest (loop [acc []
               fuel input]
          (if (<= fuel 0)
            acc
            (recur (conj acc fuel) (- (int (/ fuel 3)) 2))))))

(defn get-all-fuel [input]
  (reduce +
          (flatten
            (reduce
              #(conj (recursive-fuel-req %2) %1) '() input))))