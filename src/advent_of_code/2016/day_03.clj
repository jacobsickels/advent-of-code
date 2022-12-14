(ns advent-of-code.2016.day-03
  (:require [advent-of-code.shared.read-file :as read]))

(defn day-3 []
  (let [data (read/read-file "resources/2016-3.txt")
        clean (map (fn [l] (sort (map #(Integer/parseInt %)
                                      (remove empty? (str/split (str/trim l) #" ")))))
                   data)]
    (filter (fn [[x y z]] (> (+ x y) z)) clean)))

(defn day-3-2 []
  (let [data (read/read-file "resources/2016-3.txt")
        clean (map (fn [l] (map #(Integer/parseInt %)
                                (remove empty? (str/split (str/trim l) #" "))))
                   data)
        clean-test [[101 301 501]
                    [102 302 502]
                    [103 303 503]
                    [201 401 601]
                    [202 402 602]
                    [203 403 603]]]
    (filter
      (fn [[x y z]] (> (+ x y) z))
      (partition 3 (flatten (map #(list (sort (map (fn [l] (nth l 0)) %))
                                        (sort (map (fn [l] (nth l 1)) %))
                                        (sort (map (fn [l] (nth l 2)) %)))
                                 (partition 3 clean)))))))