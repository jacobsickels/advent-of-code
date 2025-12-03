(ns advent-of-code.2017.day-16
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (-> (read/read-file "resources/2017/day_16.txt")
              (first)
              (str/split #",")))

(def start (into {} (map-indexed (fn [idx n] [idx n]) [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p])))

(defn spin [acc size]
  (let [current-order (->> (sort-by first acc)
                           (map second))]
    (->> (concat (take-last size current-order) (take (- 16 size) current-order))
         (map-indexed (fn [idx n] [idx n]))
         (into {}))))

(defn exchange [acc pos-a pos-b]
  (assoc acc pos-a (get acc pos-b) pos-b (get acc pos-a)))

(defn partner [acc val-a val-b]
  (let [inverted-acc (set/map-invert acc)
        pos-a (get inverted-acc (keyword val-a))
        pos-b (get inverted-acc (keyword val-b))]
    (exchange acc pos-a pos-b)))

(defn do-instruction [acc instruction]
  (let [[i & n] (str/split instruction #"")
        [a b] (str/split (apply str n) #"/")]
    (cond
      (= "s" i) (spin acc (Integer/parseInt a))
      (= "x" i) (exchange acc (Integer/parseInt a) (Integer/parseInt b))
      (= "p" i) (partner acc a b))))

(defn part-1 [input]
  (loop [values input
         instructions data]
    (if (empty? instructions)
      values
      (let [next-values (do-instruction values (first instructions))]
        (recur next-values (rest instructions))))))


;; I used found here to find how many itterations it takes to repeat (which was 60)
;; I then took (mod 1000000000 60) to get 40 which would be the iteration it was at 1 billion
(defn part-2 []
  (loop [values start
         found #{}
         ittr 0]
    (cond
      (= ittr 40)
      (->> (vec values)
           (sort-by first)
           (map second))

      :else
      (let [dance (part-1 values)
            order (->> (vec dance)
                       (sort-by first)
                       (map second))]
        (if (contains? found order)
          ittr
          (recur (part-1 values)
                 (conj found order)
                 (inc ittr)))))))