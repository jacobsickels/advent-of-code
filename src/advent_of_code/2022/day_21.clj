(ns advent-of-code.2022.day-21
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (read/read-file "resources/2022/day_21.txt"))

(defn parse [col]
  (reduce
    (fn [acc line] (let [[monkey job] (str/split line #": ")
                         [a b c] (str/split job #" ")
                         func (get {"*" * "/" / "-" - "+" +} b)]

                     (if (nil? b)
                       (assoc acc monkey { :value (Integer/parseInt a)})
                       (assoc acc monkey {:deps [a c] :job (fn [[a c]] (func a c)) :value nil}))))
    {}
    col))

(defn evaluate-monkey [monkeys monkey]
  (let [monkey-structure (get monkeys monkey)]
    (if (:value monkey-structure)
      monkeys
      (let [values (map #(:value (get monkeys %)) (:deps monkey-structure))
            has-needed-values? (every? some? values)]
        (if has-needed-values?
          (do
            (when (and (integer? (first values)) (= monkey "root")) (println values))
            (assoc-in monkeys [monkey :value] ((:job monkey-structure) values)))
          monkeys)))))

(defn parse-monkeys-once [monkeys]
  (loop [check-monkeys (keys monkeys)
         new-monkeys monkeys]
    (if (empty? check-monkeys)
      new-monkeys
      (recur (rest check-monkeys)
             (evaluate-monkey new-monkeys (first check-monkeys))))))

(defn all-monkeys-have-value? [monkeys]
  (= (count monkeys) (count (filter (fn [[k v]] (:value v)) monkeys))))

(defn part-1 [col]
  (loop [monkeys (parse col)]
    (if (all-monkeys-have-value? monkeys)
      monkeys
      (recur (parse-monkeys-once monkeys)))))

(defn human-yelling [col input]
  (let [monkeys (parse col)
        monkeys (assoc monkeys "root" (assoc (get monkeys "root") :job (fn [[a c]] (= a c))))
        monkeys (assoc monkeys "humn" {:value input})]
    (loop [mks monkeys]
      (if (all-monkeys-have-value? (dissoc mks "root"))
        (:value (get (evaluate-monkey mks "root") "root"))
        (recur (parse-monkeys-once mks))))))

(defn part-2 [col]
  (loop [check-number 3592056845000] ;; 3592056845086
    (println check-number)
    (if (human-yelling col check-number)
      check-number
      (recur (inc check-number)))))


