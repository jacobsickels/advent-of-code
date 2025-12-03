(ns advent-of-code.2015.day-19
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2015/day_19.txt")
               (partition-by empty?)
               (remove #(= "" (first %)))))

(def replacements (->> (first data)
                       (map #(str/split % #" => "))))

(def check-string (->> (first (last data))))

(defn get-replacements [col pos]
  (let [single-chars (subs col pos (inc pos))
        double-chars (if (<= (+ 2 pos) (count col)) (subs col pos (+ 2 pos)) nil)
        single-replacements (filter #(= (first %) single-chars) replacements)
        double-replacements (filter #(= (first %) double-chars) replacements)]
    (cond-> []

            (not-empty single-replacements)
            (concat (map #(let [splitted (split-at (inc pos) col)]
                            (str (apply str (butlast (first splitted)))
                                 (last %)
                                 (apply str (last splitted))))
                         single-replacements))

            (not-empty double-replacements)
            (concat (map #(let [splitted (split-at (+ pos 2) col)]
                            (str (apply str (butlast (butlast (first splitted))))
                                 (last %)
                                 (apply str (last splitted))))
                         double-replacements)))))


(defn part-1 []
  (let [col check-string]
    (loop [pos 0
           acc []]
      (if (= pos (count col))
        (count (set acc))
        (let [replacements (get-replacements col pos)]
          (recur (inc pos) (concat acc replacements)))))))

;
;(defn get-next-strings [input]
;  (loop [pos 0
;         acc []]
;    (if (= pos (count input))
;      (set acc)
;      (let [replacements (get-replacements input pos)]
;        (recur (inc pos) (concat acc replacements))))))
;
;(defn part-2 []
;  (loop [acc #{"e"}
;         cnt 0]
;    (println cnt)
;    (if (contains? acc check-string)
;      cnt
;      (recur (set (mapcat get-next-strings acc)) (inc cnt)))))

