(ns advent-of-code.2021.day-24
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def variables {:w 0 :x 0 :y 0 :z 0})

(defn is-variable? [input]
  (contains? (set (keys variables)) (keyword input)))

(defn var-or-num [variables input]
  (if (is-variable? input) (get variables input) input))

(defn parse-instruction [instruction]
  (let [[cmd f s] (str/split instruction #" ")]
    (cond
      (= cmd "inp")
      {:cmd cmd :variable (if (is-variable? f) (keyword f) (Integer/parseInt f))}

      :else
      {:cmd cmd
       :left (if (is-variable? f) (keyword f) (Integer/parseInt f))
       :right (if (is-variable? s) (keyword s) (Integer/parseInt s))})))

(def data (->> (read/read-file "resources/2021/day_24.txt")
               (map parse-instruction)))



(defn do-instruction [inputs variables instruction]
  (cond
    (= "inp" (:cmd instruction))
    [(rest inputs) (merge variables {(:variable instruction) (first inputs)})]

    (= "add" (:cmd instruction))
    [inputs (merge variables {(:left instruction) (+ (var-or-num variables (:left instruction))
                                                     (var-or-num variables (:right instruction)))})]

    (= "mul" (:cmd instruction))
    [inputs (merge variables {(:left instruction) (* (var-or-num variables (:left instruction))
                                                     (var-or-num variables (:right instruction)))})]

    (= "div" (:cmd instruction))
    [inputs (merge variables {(:left instruction) (Math/floorDiv (var-or-num variables (:left instruction))
                                                                 (var-or-num variables (:right instruction)))})]
    (= "mod" (:cmd instruction))
    [inputs (merge variables {(:left instruction) (mod (var-or-num variables (:left instruction))
                                                       (var-or-num variables (:right instruction)))})]
    (= "eql" (:cmd instruction))
    [inputs (merge variables {(:left instruction) (if (= (var-or-num variables (:left instruction))
                                                         (var-or-num variables (:right instruction)))
                                                    1
                                                    0)})]))

(defn digits [n]
  (if (pos? n)
    (conj (digits (quot n 10)) (mod n 10))
    []))

(defn get-next-model [input]
  (->> (digits (inc input))
       (map #(if (zero? %) 1 %))
       (reduce (fn [acc n] (+ (* 10 acc) n)))))

(defn monad [input]
  (loop [inputs input
         vars variables
         instructions data]
    (if (empty? instructions)
      (:z vars)
      (let [[new-inputs new-vars] (do-instruction inputs vars (first instructions))]
        (println (first instructions) new-vars)
        (recur new-inputs
                new-vars
                (rest instructions))))))

(defn part-1 []
  (loop [check-number 11111111111111]
    (let [model-output (monad (digits check-number))]
      (if (zero? model-output)
        check-number
        (recur (get-next-model check-number))))))

