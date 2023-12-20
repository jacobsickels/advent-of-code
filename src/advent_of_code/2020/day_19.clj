(ns advent-of-code.2020.day-19
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def test-rules (->> (read/read-file "resources/2020/2020-19-example-rules.txt")
                     (map #(let [[k v] (str/split % #": ")]
                             (if (re-find #"[ab]" v)
                               {k {:value (first (re-seq #"[ab]" v))}}
                               {k {:value nil :children (->> (str/split v #" | ")
                                                             (partition-by (fn [c] (= "|" c)))
                                                             (remove (fn [col] (= (first col) "|"))))}})))
                     (apply merge)))

(def rules (->> (read/read-file "resources/2020/day-19-rules.txt")
                (map #(let [[k v] (str/split % #": ")]
                        (if (re-find #"[ab]" v)
                          {k {:value (first (re-seq #"[ab]" v))}}
                          {k {:value nil :children (->> (str/split v #" | ")
                                                        (partition-by (fn [c] (= "|" c)))
                                                        (remove (fn [col] (= (first col) "|"))))}})))
                (apply merge)))
(def test-data (read/read-file "resources/2020/2020-19-example-data.txt"))
(def data (read/read-file "resources/2020/day-19-data.txt"))


(defn build-regex [rules key]
  (let [rule (get rules key)]
    (if (contains? #{"a" "b"} (:value rule))
      (:value rule)
      (str "("
           (->> (map (fn [col]
                       (->> (map (fn [c] (build-regex rules c)) col)
                            (str/join "")))
                     (:children rule))
                (str/join "|"))
           ")"))))


(defn part-1 []
  (let [regex (build-regex rules "0")]
    (->> (map #(list % (first (re-matches (re-pattern regex) %))) data)
         (filter (fn [[f s]] (= f s)))
         count)))

(defn part-1-test []
  (let [regex (build-regex test-rules "0")]
    (->> (map #(list % (first (re-matches (re-pattern regex) %))) test-data)
         (filter (fn [[f s]] (= f s)))
         count)))

(declare memoized-build-regex-2)

(defn build-regex-2 [rules key]
  (cond
    (= key "8")
    (str (memoized-build-regex-2 rules "42") "+")

    (= key "11")
    (let [fourtytwo (memoized-build-regex-2 rules "42")
          thirtyone (memoized-build-regex-2 rules "31")]
      (str "("
           (->> (map #(str "(" fourtytwo "{" % "}" thirtyone "{" % "}" ")") (range 1 4))
                (str/join "|"))
           ")"))

    :else (let [rule (get rules key)]
            (if (contains? #{"a" "b"} (:value rule))
              (:value rule)
              (str "("
                   (->> (map (fn [col]
                               (->> (map (fn [c] (memoized-build-regex-2 rules c)) col)
                                    (str/join "")))
                             (:children rule))
                        (str/join "|"))
                   ")")))))

(def memoized-build-regex-2 (memoize build-regex-2))

(defn part-2-test []
  "must replace rules in build-regex-2 with test-rules"
  (let [regex (build-regex-2 test-rules "0")]
    (->> (map #(list % (first (re-matches (re-pattern regex) %))) test-data)
         (filter (fn [[f s]] (= f s)))
         count)))

(defn part-2 []
  (let [regex (build-regex-2 rules "0")]
    (->> (map #(list % (first (re-matches (re-pattern regex) %))) data)
         (filter (fn [[f s]] (= f s)))
         count)))