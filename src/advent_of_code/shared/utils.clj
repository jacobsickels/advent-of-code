(ns advent-of-code.shared.utils)

(defn inclusive-range [start end]
  (range start (inc end)))


(defn parse-int-col [col]
  (map #(Integer/parseInt %) col))

(defn segmentize [cmp col]
  (let [switch (reductions = true (map cmp col (rest col)))]
    (map (partial map first) (partition-by second (map list col switch)))))

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))