(ns advent-of-code.2018.day-04
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))



(defn- get-data []
  (let [data (read/read-file "resources/2018/day_04.txt")]
    (->> (map #(let [[_ date] (re-find #"\[(.+?)\]" %)
                     guard (re-find #"#\d+" %)]
                 {:date   date
                  :guard  guard
                  :action (cond
                            (str/includes? % "begins") :begin
                            (str/includes? % "wakes") :wake
                            (str/includes? % "falls") :asleep
                            :else :nothing)})
              data)
         (sort-by :date))))

(defn- get-guard-times [col]
  (:guards (reduce (fn [acc action]
                     (if (and (not (contains? (:guards acc) (:guard action))) (seq (:guard action)))
                       (merge (assoc-in acc [:guards (:guard action)] [])
                              {:last-guard (:guard action)})
                       (cond
                         (= (:action action) :asleep)
                         (update-in acc [:guards (:last-guard acc)] conj (:date action))

                         (= (:action action) :wake)
                         (update-in acc [:guards (:last-guard acc)] conj (:date action))

                         (= (:action action) :begin)
                         (assoc acc :last-guard (:guard action))

                         :else acc)))
                   {:last-guard nil :guards {}}
                   col)))

(defn- get-minutes-between [[time-1 time-2]]
  (let [get-minutes (fn [time] (->> (take-last 2 time)
                                    (apply str)
                                    (Integer/parseInt)))
        time-1-minutes (get-minutes time-1)
        time-2-minutes (get-minutes time-2)]
    (range time-1-minutes time-2-minutes)))


(defn- keywordize-numbers [my-map]
  (into {}
        (for [[k v] my-map]
          [(keyword (str k)) v])))

(defn- get-guards-minutes-asleep []
  (->> (get-data)
       get-guard-times
       (reduce-kv (fn [acc k v]
                    (let [asleep-times (partition 2 v)]
                      (assoc acc k (keywordize-numbers (frequencies (mapcat get-minutes-between asleep-times))))))
                  {})))


(defn- part-1 []
  (->> (get-guards-minutes-asleep)
       (reduce-kv (fn [acc k v]
                    (let [answer (when (seq v) (let [[minute minutes-asleep] (apply max-key val v)]
                                                 {:minute minute :asleep minutes-asleep}))]
                      (conj acc {:guard          k
                                 :minutes-asleep (reduce + (vals v))
                                 :minute         (:minute answer)
                                 :minute-asleep-time         (:asleep answer)})))
                  [])
       (sort-by :minutes-asleep)
       last))

(defn- part-2 []
  (->> (get-guards-minutes-asleep)
       (reduce-kv (fn [acc k v]
                    (let [answer (when (seq v) (let [[minute minutes-asleep] (apply max-key val v)]
                                                 {:minute minute :asleep minutes-asleep}))]
                      (conj acc {:guard          k
                                 :minutes-asleep (reduce + (vals v))
                                 :minute         (:minute answer)
                                 :minute-asleep-time         (:asleep answer)})))
                  [])
       (sort-by :minute-asleep-time)
       last))