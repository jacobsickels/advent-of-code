(ns advent-of-code.2017.day-15)

(def remainder 2147483647)

(def factor-a 16807)
(def factor-b 48271)
(def input-a 516)
(def input-b 190)


(defn get-last-16 [n]
  (bit-and n 0xFFFF))

(defn compare-n [a b]
  (= (get-last-16 a) (get-last-16 b)))

(def memoized-compare (memoize compare-n))

(defn part-1 []
  (loop [gen-a input-a
         gen-b input-b
         times-equal 0
         ittr 0]
    (if (= ittr 40000000)
      times-equal
      (let [a-val (mod (* factor-a gen-a) remainder)
            b-val (mod (* factor-b gen-b) remainder)]
        (recur a-val b-val
               (if (memoized-compare a-val b-val)
                 (do
                   (println ittr)
                   (inc times-equal)) times-equal)
               (inc ittr))))))

(defn get-modded-value [n factor m]
  (loop [check n]
    (if (zero? (mod check m))
      check
      (recur (mod (* factor check) remainder)))))

(defn part-2 []
  (loop [gen-a input-a
         gen-b input-b
         times-equal 0
         ittr 0]
    (if (= ittr 5000000)
      times-equal
      (let [a-val (get-modded-value (mod (* factor-a gen-a) remainder) factor-a 4)
            b-val (get-modded-value (mod (* factor-b gen-b) remainder) factor-b 8)]
        (recur a-val b-val
               (if (memoized-compare a-val b-val)
                 (do
                   (println ittr)
                   (inc times-equal)) times-equal)
               (inc ittr))))))