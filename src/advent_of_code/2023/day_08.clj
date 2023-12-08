(ns advent-of-code.2023.day-08
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(def navigation (->> (read/read-file "resources/2023/day_08.txt")
                     first))

(def routes (->> (read/read-file "resources/2023/day_08.txt")
                 (drop 2)
                 (reduce #(let [[k lr] (str/split %2 #" = ")
                                [l r] (str/split lr #", ")]
                            (assoc %1 (keyword k) [(keyword (subs l 1)) (keyword (apply str (butlast r)))]))
                         {})))

(defn cycle-map [start-location fn-end]
  (loop [location start-location
         steps 0]
    (if (fn-end location)
      steps
      (cond
        (= \R (nth navigation (mod steps (count navigation))))
        (recur (second (get routes location)) (inc steps))

        (= \L (nth navigation (mod steps (count navigation))))
        (recur (first (get routes location)) (inc steps))))))

(defn part-1 []
  (cycle-map :AAA (fn [location] (= location :ZZZ))))

(def keys-end-in-a (->> (keys routes)
                        (map name)
                        (filter #(= \A (last %)))
                        (map keyword)))

(defn part-2 []
  (->> (map #(cycle-map % (fn [location] (= \Z (last (name location))))) keys-end-in-a)
       (apply utils/lcmv)))