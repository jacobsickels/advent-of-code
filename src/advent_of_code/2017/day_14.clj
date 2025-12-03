(ns advent-of-code.2017.day-14)

(defn string->binary [s]
  (apply str (map #(Integer/toBinaryString (int %)) s)))
