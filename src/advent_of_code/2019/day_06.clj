(ns advent-of-code.2019.day-06
  (:require [clojure.string :as str]))

(defn format-data [data]
  (map #(str/split % #"[)]") data))

(defn get-file-contents []
  (format-data
    (with-open [reader (clojure.java.io/reader "resources/day_6_input.txt")]
      (reduce conj [] (line-seq reader)))))


(defn get-direct-orbit [list find]
  (first (first (filter (fn [[_ y]] (= y find)) list))))


(defn get-total-orbits [list find]
  (loop [orbit (get-direct-orbit list find)
         count 0]
    (if (nil? orbit)
      count
      (recur (get-direct-orbit list orbit) (inc count)))))


(defn get-total [list]
  (reduce + (map #(get-total-orbits list %) (set (flatten list)))))


(defn get-direct-orbit-line
  [list find]
  (loop [orbit (get-direct-orbit list find)
         acc []]
    (if (nil? orbit)
      acc
      (recur (get-direct-orbit list orbit) (conj acc orbit)))))

(defn get-orbit-intersection [list find1 find2]
  (let [find1-list (get-direct-orbit-line list find1)
        find2-list (get-direct-orbit-line list find2)]
    (loop [list find1-list]
      (if (contains? (set find2-list) (first list))
        (first list)
        (recur (rest list))))))


(defn get-count-to-body [list find body]
  (loop [orbit (get-direct-orbit list find)
         count 0]
    (if (= orbit body)
      count
      (recur (get-direct-orbit list orbit) (inc count)))))


(defn find-count-to-friend [list friend]
  (let [intersection (get-orbit-intersection list "YOU" friend)
        you-count (get-count-to-body list "YOU" intersection)
        friend-count (get-count-to-body list friend intersection)]
    (+ you-count friend-count)))