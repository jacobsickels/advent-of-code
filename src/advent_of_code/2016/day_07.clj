(ns advent-of-code.2016.day-07
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(defn contains-abba? [line]
  (or (some true? (map (fn [[a b c d]]
                         (and (= a d)
                              (= b c)
                              (not (= a b))))
                       (partition 4 1 line))) false))

(defn is-valid-ipv7? [line]
  (let [splitted (str/split line #"[\[\]]")
        hypernets (take-nth 2 (rest splitted))
        others (take-nth 2 splitted)
        valid-hypernet? (every? false? (map contains-abba? hypernets))
        valid-others? (some true? (map contains-abba? others))]
    (and valid-hypernet? valid-others?)))

(defn day-7 []
  (let [data (read/read-file "resources/2016-7.txt")]
    (count (filter true? (map is-valid-ipv7? data)))))

(defn contains-aba? [line]
  (remove nil? (map (fn [[a b c]] (if (and (= a c) (not (= a b))) (str a b c)))
                    (partition 3 1 line))))

(defn contains-a-bab? [line abas]
  (let [babs (map #(let [[a b a] (seq %)] (str b a b)) abas)]
    (not (empty?
           (set/intersection (set (map #(apply str %) (partition 3 1 line))) (set babs))))))


(defn can-support-ssl? [line]
  (let [splitted (str/split line #"[\[\]]")
        hypernets (take-nth 2 (rest splitted))
        others (take-nth 2 splitted)
        abas (flatten (map contains-aba? others))
        valid-hypernet (map #(contains-a-bab? % abas) hypernets)]
    (some true? valid-hypernet)))


(defn day-7-2 []
  (let [data (readread-file "resources/2016-7.txt")]
    (count (filter true? (map can-support-ssl? data)))))