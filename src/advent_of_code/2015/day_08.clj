(ns advent-of-code.2015.day-08
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- count-character-literals [col]
  (count col))

(defn unescape-quotes [s]
  (let [no-quotes (-> (str/replace s #"\\\"" "~"))
        no-outer-quotes (str/replace (->> no-quotes
                                          (drop-last 1)
                                          (drop 1)
                                          (apply str))
                                     #"~" "\"")]
    no-outer-quotes))

(defn unescape-backslash [s]
  (str/replace s #"\\\\" "%"))

(defn replace-all-hex [s]
  (str/replace s #"\\x[0-9a-fA-F][0-9a-fA-F]" "@"))

(defn sum-counts [col]
  (reduce (fn [acc item] (+ acc (- (:literal item) (:count item))))
          0
          col))

(defn sum-counts-hex [col]
  (reduce (fn [acc item] (+ acc (- (:literal item) (:count-hex item))))
          0
          col))

(defn make-map [s]
  {:literal     (count-character-literals s)
   :string      (str/trim s)
   :without-hex (unescape-backslash (replace-all-hex (unescape-quotes s)))
   :count       (count (unescape-backslash (replace-all-hex (unescape-quotes s))))
   :count-hex (count (unescape-backslash (unescape-quotes s)))})

(defn part-1 []
  (let [data (read/read-file "resources/2015/day_08.txt")]
    (map #(make-map %) data)))

(defn part-2 []
  (let [data (read/read-file "resources/2015/day_08_part2.txt")]
    (map #(make-map %) data)))

;; pr-str will do what I did with rereading the file instead

;(defn part-2 []
;  (let [data (read/read-file "resources/2015/day_08.txt")]
;    (->> (map #(:string (make-map %)) data)
;         (map pr-str))))