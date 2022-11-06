(ns advent-of-code.2018.day-03
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn- get-data []
  (let [data (read/read-file "resources/2018/day_03.txt")]
    (->> (map #(str/split % #" @ ") data)
         (map (fn [[claim-id area]]
                (let [[point-str area-str] (str/split area #": ")
                      point (read-string (str "[" point-str "]"))
                      area (read-string (str "[" (str/replace area-str #"x" ",") "]"))]
                  {:claim-id claim-id :point point :area area}))))))

(defn- make-claim-points [claim]
  (let [[x y] (:point claim)
        [w h] (:area claim)]
    (combo/cartesian-product (range x (+ x w)) (range y (+ y h)))))


(defn- new-fabric [fabric claim]
  (let [claim-points (make-claim-points claim)]
    {:claims        (set/union (:claims fabric) claim-points)
     :intersections (set/difference (:intersections fabric) claim-points)}))

(defn- get-intersection [claim col]
  (count (apply set/union (map #(set/intersection (:points claim) (:points %))
                               (remove #(= (:claim-id claim) (:claim-id %)) col)))))

(defn- point-not-in-claim? [point claim]
  (empty? (set/intersection #{point} (set (:points claim)))))

(defn- point-in-multiple-claims? [point claims]
  (some #(not (point-not-in-claim? point %)) claims))

(defn get-claim-point-frequencies []
  (loop [checking (get-data)
         freq {}]
    (if (empty? checking)
      freq
      (recur (rest checking) (merge-with + freq (frequencies (make-claim-points (first checking))))))))

(def data (get-claim-point-frequencies))
(def claims (get-data))

(defn- part-1 []
  (let [freq data]
    (count (reduce-kv (fn [acc k v]
                        (if (> v 1) (assoc acc k v) acc)) {} freq))))

(def single-frequencies (reduce-kv (fn [acc k v]
                                     (if (= v 1) (assoc acc k v) acc)) {} data))

(def single-frequencies-points (set (keys single-frequencies)))

(defn claim-in-frequencies [claim]
  (let [claim-points (set (make-claim-points claim))]
    (set/subset? claim-points single-frequencies-points)))

(defn- part-2 []
  (filter claim-in-frequencies claims))