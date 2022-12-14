(ns advent-of-code.2021.day-12
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(def test-data ["start-A"
                "start-b"
                "A-c"
                "A-b"
                "b-d"
                "A-end"
                "b-end"])

(def test-data-2 ["dc-end"
                  "HN-start"
                  "start-kj"
                  "dc-start"
                  "dc-HN"
                  "LN-dc"
                  "HN-end"
                  "kj-sa"
                  "kj-HN"
                  "kj-dc"])

(def test-data-3 ["fs-end"
                  "he-DX"
                  "fs-he"
                  "start-DX"
                  "pj-DX"
                  "end-zg"
                  "zg-sl"
                  "zg-pj"
                  "pj-he"
                  "RW-he"
                  "fs-DX"
                  "pj-RW"
                  "zg-RW"
                  "start-pj"
                  "he-WI"
                  "zg-he"
                  "pj-fs"
                  "start-RW"])

(def data ["DA-xn"
           "KD-ut"
           "gx-ll"
           "dj-PW"
           "xn-dj"
           "ll-ut"
           "xn-gx"
           "dg-ak"
           "DA-start"
           "ut-gx"
           "YM-ll"
           "dj-DA"
           "ll-xn"
           "dj-YM"
           "start-PW"
           "dj-start"
           "PW-gx"
           "YM-gx"
           "xn-ak"
           "PW-ak"
           "xn-PW"
           "YM-end"
           "end-ll"
           "ak-end"
           "ak-DA"])

(defn get-caves [input]
  (set (flatten (map #(str/split % #"-") input))))

(defn key-is-lower-chars? [k]
  (= (name k) (str/lower-case (name k))))

(defn path-contains-double-lower? [path]
  (not (empty? (filter (fn [[k v]]
                         (and (key-is-lower-chars? k) (>= v 2)))
                       (frequencies path)))))

(defn get-connections [input]
  (apply merge (map (fn [c]
                      (reduce
                        (fn [acc [s e]] (cond
                                          (= s c)
                                          (assoc acc (keyword c) (conj (get acc (keyword c)) (keyword e)))

                                          (= e c)
                                          (assoc acc (keyword c) (conj (get acc (keyword c)) (keyword s)))

                                          :else
                                          acc))
                        {}
                        (map #(str/split % #"-") input)))
                    (get-caves input))))

(defn find-paths [lookup connections]
  (cond
    (contains? #{:end} (last lookup))
    [lookup]

    :else
    (map #(conj lookup %) (get connections (last lookup)))))

(defn remove-bad-paths [paths]
  (remove #(path-contains-double-lower? %) paths))

(defn find-path-iteration [paths connections]
  (remove-bad-paths (reduce
                      (fn [acc v] (reduce conj acc (find-paths v connections)))
                      []
                      paths)))

(defn part-1 [input]
  (let [connections (get-connections input)]
    (loop [acc      (find-path-iteration (find-paths [:start] connections) connections)
           last-acc nil]
      (if (= (set acc) (set last-acc))
        (count acc)
        (recur (find-path-iteration acc connections) acc)))))


;; Part 2

(defn path-is-valid? [path]
  (let [freq (frequencies path)
        double-keys (filter (fn [[k v]]
                              (and (key-is-lower-chars? k) (>= v 2)))
                            freq)]
    
    (and (<= (or (:start freq) 0) 1)
         (<= (or (:end freq) 0) 1)
         (<= (count double-keys) 1)
         (if (= 1 (count double-keys))
           (<= (second (first double-keys)) 2)
           true))))
         


(defn remove-bad-paths-2 [paths]
  (filter #(path-is-valid? %) paths))

(defn find-path-iteration-2 [paths connections]
  (remove-bad-paths-2 (reduce
                        (fn [acc v] (reduce conj acc (find-paths v connections)))
                        []
                        paths)))

(defn part-2 [input]
  (let [connections (get-connections input)]
    (loop [acc      (find-path-iteration-2 (find-paths [:start] connections) connections)
           last-acc nil]
      (if (= (set acc) (set last-acc))
        (count acc)
        (recur (find-path-iteration-2 acc connections) acc)))))
  
