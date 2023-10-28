(ns advent-of-code.2020.day-19
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def test-rules ["0: 4 1 5"
                 "1: 2 3 | 3 2"
                 "2: 4 4 | 5 5"
                 "3: 4 5 | 5 4"
                 "4: \"a\""
                 "5: \"b\""])


(defn format-data [input]
  (apply merge (map #(let [[k v] (str/split % #": ")]
                       (if (re-find #"[ab]" v)
                         {k {:value (first (re-seq #"[ab]" v))}}
                         {k {:value nil :regex (str/replace v #" " "") :deps (set (re-seq #"\d+" v))}}))
                    input)))

(def formatted (format-data (read/read-file "resources/2020/day-19-rules.txt")))
;(def formatted (format-data test-rules))

(defn replace-regex-with-deps-value [k formatted]
  (let [replace-pattern (reduce (fn [acc item] (assoc acc item (:value (get formatted item)))) {} (:deps (get formatted k)))]
    {k (merge (get formatted k)
              {:value (str "("
                           (str/replace (:regex (get formatted k))
                                        (re-pattern (str/join "|" (:deps (get formatted k))))
                                        replace-pattern)
                           ")")})}))

(defn deps-have-value? [k formatted]
  (let [deps (:deps (get formatted k))]
    (if (nil? deps)
      false
      (->> (map #(:value (get formatted %)) deps)
           (every? #(not (nil? %)))))))

(defn make-regex-for-deps [formatted]
  (reduce (fn [acc [k v]]
            (if (deps-have-value? k formatted)
              (merge acc (replace-regex-with-deps-value k formatted))
              (assoc acc k v)))
          {}
          formatted))

(defn iterate-until-zero-has-value []
  (loop [rules formatted]
    (if (:value (get rules "0"))
      rules
      (recur (make-regex-for-deps rules)))))


(defn part-1 []
  (let [rules (iterate-until-zero-has-value)
        check-regex (get-in rules ["0" :value])
        to-check (read/read-file "resources/2020/day-19-data.txt")]
    (->> (map #(list % (first (re-matches (re-pattern check-regex) %))) to-check)
         (filter (fn [[f s]] (= f s)))
         count)))