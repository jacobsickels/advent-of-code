(ns advent-of-code.2022.day-11)

(def real-monkeys [{:starting [73 77]
                    :op (fn [old] (* 5 old))
                    :test (fn [input] (if (zero? (mod input 11)) 6 5))
                    :inspected 0}
                   {:starting [57 88 80]
                    :op (fn [old] (+ 5 old))
                    :test (fn [input] (if (zero? (mod input 19)) 6 0))
                    :inspected 0}
                   {:starting [61, 81, 84, 69, 77, 88]
                    :op (fn [old] (* 19 old))
                    :test (fn [input] (if (zero? (mod input 5)) 3 1))
                    :inspected 0}
                   {:starting [78, 89, 71, 60, 81, 84, 87, 75]
                    :op (fn [old] (+ 7 old))
                    :test (fn [input] (if (zero? (mod input 3)) 1 0))
                    :inspected 0}
                   {:starting [60, 76, 90, 63, 86, 87, 89]
                    :op (fn [old] (+ 2 old))
                    :test (fn [input] (if (zero? (mod input 13)) 2 7))
                    :inspected 0}
                   {:starting [88]
                    :op (fn [old] (+ 1 old))
                    :test (fn [input] (if (zero? (mod input 17)) 4 7))
                    :inspected 0}
                   {:starting [84, 98, 78, 85]
                    ;; Secret sauce for part 2 is here
                    ;; mod this specific op by the lcm if all the mod checks from all the monkeys
                    ;; this lowers the value but won't change the mod arithmetic for the other monkeys
                    :op (fn [old] (mod (* old old) 9699690))
                    :test (fn [input] (if (zero? (mod input 7)) 5 4))
                    :inspected 0}
                   {:starting [98, 89, 78, 73, 71]
                    :op (fn [old] (+ 4 old))
                    :test (fn [input] (if (zero? (mod input 2)) 3 2))
                    :inspected 0}])

(def test-monkeys [{:starting [79, 98]
                    :op (fn [old] (* 19 old))
                    :test (fn [input] (if (zero? (mod input 23)) 2 3))
                    :inspected 0}
                   {:starting [54, 65, 75, 74]
                    :op (fn [old] (+ 6 old))
                    :test (fn [input] (if (zero? (mod input 19)) 2 0))
                    :inspected 0}
                   {:starting [79, 60, 97]
                    ;; Secret sauce for part 2 is here
                    ;; mod this specific op by the lcm if all the mod checks from all the monkeys
                    ;; this lowers the value but won't change the mod arithmetic for the other monkeys
                    :op (fn [old] (mod (* old old) 96577))
                    :test (fn [input] (if (zero? (mod input 13)) 1 3))
                    :inspected 0}
                   {:starting [74]
                    :op (fn [old] (+ 3 old))
                    :test (fn [input] (if (zero? (mod input 17)) 0 1))
                    :inspected 0}])

(defn monkey-receives-item [monkeys monkey-num item]
  (assoc monkeys monkey-num (update (nth monkeys monkey-num) :starting concat [item])))

(defn monkey-throws-item [monkeys monkey-num]
  (assoc monkeys monkey-num (update (nth monkeys monkey-num) :starting rest)))

(defn monkey-inspected-item [monkeys monkey-num]
  (assoc monkeys monkey-num (update (nth monkeys monkey-num) :inspected inc)))

(defn monkey-round [monkeys]
  (loop [mks monkeys
         monkey-num 0]
    (if (= monkey-num (count monkeys))
      mks
      (let [operating-monkey (nth mks monkey-num)]
        (cond
          (empty? (:starting operating-monkey))
          (recur mks (inc monkey-num))

          :else
          (let [inspected ((:op operating-monkey) (first (:starting operating-monkey)))
                less-worry-item (Math/floorDiv inspected 3)
                thrown-to-monkey ((:test operating-monkey) less-worry-item)]
            (recur (-> mks
                       (monkey-inspected-item monkey-num)
                       (monkey-throws-item monkey-num)
                       (monkey-receives-item thrown-to-monkey less-worry-item))
                   monkey-num)))))))

(defn part-1 [monkeys]
  (loop [mks monkeys
         rounds 0]
    (if (= rounds 20)
      (->> mks
           (sort-by :inspected)
           (take-last 2)
           (map :inspected)
           (reduce *))
      (recur (monkey-round mks) (inc rounds)))))

(defn monkey-round-2 [monkeys]
  (loop [mks monkeys
         monkey-num 0]
    (if (= monkey-num (count monkeys))
      mks
      (let [operating-monkey (nth mks monkey-num)]
        (cond
          (empty? (:starting operating-monkey))
          (recur mks (inc monkey-num))

          :else
          (let [inspected ((:op operating-monkey) (first (:starting operating-monkey)))
                less-worry-item (biginteger inspected)
                thrown-to-monkey ((:test operating-monkey) less-worry-item)]
            (recur (-> mks
                       (monkey-inspected-item monkey-num)
                       (monkey-throws-item monkey-num)
                       (monkey-receives-item thrown-to-monkey less-worry-item))
                   monkey-num)))))))

(defn part-2 [monkeys]
  (loop [mks monkeys
         rounds 0]
    (if (= rounds 10000)
      (->> mks
           (sort-by :inspected)
           (take-last 2)
           (map :inspected)
           (reduce *))
      (recur (monkey-round-2 mks) (inc rounds)))))
