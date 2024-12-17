(ns advent-of-code.2017.day-25)

(def test-data {:a (fn [p val] (if (zero? val) [1 (inc p) :b] [0 (dec p) :b]))
                :b (fn [p val] (if (zero? val) [1 (dec p) :a] [1 (inc p) :a]))})

(def data {:a (fn [p val] (if (zero? val) [1 (inc p) :b] [1 (dec p) :e]))
           :b (fn [p val] (if (zero? val) [1 (inc p) :c] [1 (inc p) :f]))
           :c (fn [p val] (if (zero? val) [1 (dec p) :d] [0 (inc p) :b]))
           :d (fn [p val] (if (zero? val) [1 (inc p) :e] [0 (dec p) :c]))
           :e (fn [p val] (if (zero? val) [1 (dec p) :a] [0 (inc p) :d]))
           :f (fn [p val] (if (zero? val) [1 (inc p) :a] [1 (inc p) :c]))})


(defn perform-steps [data cnt]
  (loop [curr-point 0
         curr-step :a
         steps 0
         acc {0 0}]
    (if (= steps cnt)
      (reduce + (vals acc))
      (let [[write-val new-point next-step] ((get data curr-step) curr-point (get acc curr-point 0))]
        (recur new-point next-step (inc steps) (assoc acc curr-point write-val))))))