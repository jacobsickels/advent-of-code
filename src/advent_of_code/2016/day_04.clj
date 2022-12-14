(ns advent-of-code.2016.day-04
  (:require [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))

(defn parse-line [line]
  (let [parsed (str/split line #"-")
        encrypted-original (str/join "-" (butlast parsed))
        encrypted-name (apply str (butlast parsed))
        [sector-id checksum] (str/split (last parsed) #"[\[\]]")
        checksum-frequencies (flatten (map #(sort-by key %)
                                           (partition-by val (sort-by val > (frequencies encrypted-name)))))
        parsed-checksum (apply str (take 5 (take-nth 2 checksum-frequencies)))]
    (if (= parsed-checksum checksum)
      [true (Integer/parseInt sector-id) encrypted-original]
      [false (Integer/parseInt sector-id) encrypted-original])))


(defn day-4 []
  (let [data (read/read-file "resources/2016-4.txt")]
    (reduce + (map second (filter first (map parse-line data))))))

(defn decrypt-char [char sector-id]
  (mod (+ (- (int char) 97) sector-id) 26))

(defn decrypt-line [encrypted-name sector-id]
  (map #(if (= % \-) " " (decrypt-char % sector-id)) (seq encrypted-name)))

(defn translate-line [encrypted-name sector-id]
  (apply str (map #(if (= % " ") " " (char (+ 97 %))) (decrypt-line encrypted-name sector-id))))

(defn day-4-2 []
  (let [data (read/read-file "resources/2016-4.txt")]
    (filter #(str/includes? (second %) "north")
            (map (fn [[f s t]] [s (translate-line t s)])
                 (filter first (map parse-line data))))))