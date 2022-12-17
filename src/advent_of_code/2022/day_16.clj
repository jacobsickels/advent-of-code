(ns advent-of-code.2022.day-16
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(def weighted-graph
  {:s {:a 3 :d 4}
   :a {:s 3 :d 5 :b 4}
   :b {:a 4 :e 5 :c 4}
   :c {:b 4}
   :d {:s 4 :a 5 :e 2}
   :e {:d 2 :b 5 :f 4}
   :f {:e 4 :g 1}})

(defn cost-from
  "What is the edge cost of going from one vertex to another connected vertex"
  [weighted-graph start goal]
  (get-in weighted-graph [start goal]))

(defn alternating-pairs
  "Turn a sequence into alternating pairs
   i.e [:a :b :c] -> [[:a :b] [:b :c]]"
  [coll]
  (let [[x y] coll
        xs (rest coll)]
    (if (empty? xs)
      []
      (->> (conj [] [x y] (alternating-pairs xs))
           flatten
           (partition 2)))))

(defn- dfs
  [graph goal]
  (fn search
    [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> current graph keys
             (remove visited)
             (mapcat #(search (conj path %) (conj visited %))))))))

(defn find-path
  "Returns a lazy sequence of all directed paths from start to goal
  within graph."
  [graph start goal]
  ((dfs graph goal) [start] #{start}))



(defn parse-line [line]
  {:node (second (str/split line #" "))
   :rate (Integer/parseInt (first (re-seq #"\d+" (second (str/split line #"=")))))
   :deps (-> (str/split line #" valve[s]? ")
             (second)
             (str/split #", "))})

(def test-data (->> (read/read-file "resources/2022/day_16_example.txt")
                    (map parse-line)))

(def data (->> (read/read-file "resources/2022/day_16.txt")
               (map parse-line)))

(defn make-graph [data]
  (reduce (fn [acc item] (assoc acc (:node item)
                                    (reduce (fn [acc1 item1] (assoc acc1 item1 (if (zero? (:rate item)) 1 2))) {} (:deps item))))
          {}
          data))

(defn make-rates [data]
  (reduce (fn [acc item] (assoc acc (:node item) (:rate item)))
          {}
          data))

(def graph (make-graph test-data))
(def rates (make-rates test-data))

(defn next-highest-path [valve visited time]
  (println valve)
  (let [valid-paths (->> (remove #(contains? visited %) (keys rates))
                         (map #(find-path graph valve %))
                         (map #(sort-by count %))
                         (map first)
                         (reduce (fn [acc item] (assoc acc item (get rates (last item)))) {})
                         (sort-by (juxt #(count (first %))))
                         (remove #(zero? (second %)))
                         (map #(array-map :valve (last (first %)) :value (last %) :time (- time (inc (count (first %)))))))]
    valid-paths))

(defn get-cost-to-time [starting-visited starting-routes]
  (println starting-routes)
  (loop [visited starting-visited
         time-left 30
         routes starting-routes]
    (if (zero? time-left)
      routes
      (let [next-options (mapcat #(next-highest-path (:valve %) (conj visited (:valve %)) time-left) routes)]
        next-options))))



