(ns advent-of-code.2015.day-11
  (:require [clojure.set :as set]))

;; https://stackoverflow.com/questions/26082594/converting-a-number-from-base-10-to-another-base-in-clojure

(def charset "abcdefghijklmnopqrstuvwxyz")

(def num->char
  (into {} (map-indexed vector charset)))

(def char->num
  (into {} (map-indexed (comp vec reverse vector) charset)))

(def base (count charset))

(def divmod (juxt quot rem))

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
                  (recur (* x acc) (dec n)))))

(defn encode [n]
  (loop [n n a ""]
    (let [[div mod] (divmod n base)]
      (if (zero? div)
        (str (get num->char mod) a)
        (recur div (str (get num->char mod) a))))))

(defn decode-pair [idx chr]
  (* (get char->num chr)
     (exp base idx)))

(def sum (partial reduce +))

(defn decode [s]
  (sum (map-indexed decode-pair (reverse s))))

(defn increment-encoded [s]
  (encode (inc (decode s))))

(defn has-increasing-straight? [s]
  (not (empty? (filter (fn [[a b c]]
                         (let [x (get char->num a)
                               y (get char->num b)
                               z (get char->num c)]
                           (and (< x y z)
                                (= x (dec y) (- z 2)))))
                       (partition 3 1 s)))))

(def bad-letters #{\i \o \l})

(defn contains-bad-letters? [s]
  (not (empty? (set/intersection (set s) bad-letters))))

(defn contains-non-overlapping-pairs? [s]
 (->> (partition 2 1 s)
      (filter (fn [[x y]] (= x y)))
      set
      count
      (<= 2)))

(defn is-valid-password [s]
  (and (has-increasing-straight? s)
       (not (contains-bad-letters? s))
       (contains-non-overlapping-pairs? s)))

(defn part-1 []
  (loop [password (increment-encoded "cqjxjnds")]
    (if (is-valid-password password)
      password
      (recur (increment-encoded password)))))

(defn part-2 []
  (loop [password (increment-encoded "cqjxxyzz")]
    (if (is-valid-password password)
      password
      (recur (increment-encoded password)))))