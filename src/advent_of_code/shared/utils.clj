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

(defn points-between [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2)
    (map #(vec [x1 %]) (apply inclusive-range (sort [y1 y2])))

    (= y1 y2)
    (map #(vec [% y1]) (apply inclusive-range (sort [x1 x2])))))

(defn index-of [needle haystack]
  (keep-indexed #(when (= %2 needle) %1) haystack))

(defn index-exclude [r ex]
  (filter #(not (ex %)) (range r)))

(defn dissoc-idx [v & ds]
  (map v (index-exclude (count v) (into #{} ds))))