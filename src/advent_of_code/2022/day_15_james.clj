(ns advent-of-code.2022.day-15-james
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [advent-of-code.shared.read-file :as read]))
;;; "A poor man's interval tree, from http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
;;; provided the basis for this slightly enhanced version I use in Beat Link Trigger. It will allow
;;; me to keep track of ranges more efficiently than sets of individual potential beacon locations.

(defn interval-lt
  "A partial order on intervals and points, where an interval is defined
  by the vector [from to) (notation I recall meaning it includes the
  lower bound but excludes the upper bound), and either can be `nil`
  to indicate negative or positive infinity. A single point at `n` is
  represented by `[n n]`."
  [[a b] [c _]]
  (boolean (and b c
                (if (= a b)
                  (neg? (compare b c))
                  (<= (compare b c) 0)))))

(def empty-interval-map
  "An interval map with no content."
  (sorted-map-by interval-lt [nil nil] #{}))

(defn- isplit-at
  "Splits the interval map at the specified value, unless it already has
  a boundary there."
  [interval-map x]
  (if x
    (let [[[a b :as k] vs] (find interval-map [x x])]
      (if (or (= a x) (= b x))
        interval-map
        (-> interval-map (dissoc k) (assoc [a x] vs [x b] vs))))
    interval-map))

(defn matching-subsequence
  "Extracts the sequence of key, value pairs from the interval map which
  cover the supplied range (either end of which can be `nil`, meaning
  from the beginning or to the end)."
  [interval-map from to]
  (cond
    (and from to)
    (subseq interval-map >= [from from] < [to to])
    from
    (subseq interval-map >= [from from])
    to
    (subseq interval-map < [to to])
    :else
    interval-map))

(defn- ialter
  "Applies the specified function and arguments to all intervals that
  fall within the specified range in the interval map, splitting it at
  each end if necessary so that the exact desired range can be
  affected."
  [interval-map from to f & args]
  (let [interval-map (-> interval-map (isplit-at from) (isplit-at to))
        kvs          (for [[r vs] (matching-subsequence interval-map from to)]
                       [r (apply f vs args)])]
    (into interval-map kvs)))

(defn iassoc
  "Add a value to the specified range in an interval map."
  [interval-map from to v]
  (ialter interval-map from to conj v))

(defn idissoc
  "Remove a value from the specified range in an interval map. If you
  are going to be using this function much, it might be worth adding
  code to consolidate ranges that are no longer distinct."
  [interval-map from to v]
  (ialter interval-map from to disj v))

(defn iget
  "Find the values that are associated with a point or interval within
  the interval map. Calling with a single number will look up the
  values associated with just that point; calling with two arguments,
  or with an interval vector, will return the values associated with
  any interval that overlaps the supplied interval."
  ([interval-map x]
   (if (vector? x)
     (let [[from to] x]
       (iget interval-map from to))
     (iget interval-map [x x])))
  ([interval-map from to]
   (reduce (fn [result [_ vs]]
             (clojure.set/union result vs))
           #{}
           (take-while (fn [[[start]]] (< (or start (dec to)) to)) (matching-subsequence interval-map from nil)))))

;;; End of interval tree code.


(def input
  "The instructions (the puzzle input)."
  (->> (read/read-file "resources/2022/day_15.txt")))

(defn read-input
  [data]
  (->> (map (fn [line]
              (->> (re-seq #"-?\d+" line)
                   (map #(Long/parseLong %))
                   (partition 2)))
            data)))

(defn manhattan-distance
  "Calculate the manhattan distance between two points."
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn cells-too-close-set
  [[sensor-cell beacon-cell] y]
  (let [sensor-x     (first sensor-cell)
        max-distance (manhattan-distance sensor-cell beacon-cell)
        slack        (- max-distance (manhattan-distance sensor-cell [sensor-x y]))]
    (when-not (neg? slack)
      (set (for [x (range (- sensor-x slack) (inc (+ sensor-x slack)))]
             [x y])))))

(defn occupied-cells-set
  [sensors]
  (reduce (fn [acc [sensor-cell beacon-cell]]
            (set/union acc #{sensor-cell beacon-cell}))
          #{}
          sensors))

(defn count-impossible-cells-set
  [sensors y]
  (let [occupied   (occupied-cells-set sensors)
        candidates (reduce (fn [acc sensor]
                             (set/union acc (cells-too-close-set sensor y)))
                           #{}
                           sensors)]
    (count (set/difference candidates occupied))))
;; NOT 4998562

(defn count-interval-cells
  "Counts how many filled cells are represented by an interval map."
  [intervals]
  (reduce (fn [acc [[start end] value]]
            (if (get value true)
              (+ acc (- end start))
              acc))
          0
          intervals))

(defn count-beacons-on-row
  "Counts how many beacons are present on a row."
  [sensors row]
  (->> sensors
       (map second)
       (filter (fn [[_x y]] (= y row)))
       set
       count))

(defn count-impossible-cells-intervals
  [sensors y]
  (let [intervals (reduce (fn [intervals [sensor-cell beacon-cell]]
                            (let [max-distance        (manhattan-distance sensor-cell beacon-cell)
                                  [sensor-x sensor-y] sensor-cell
                                  slack               (- max-distance (Math/abs (- y sensor-y)))]
                              (if (neg? slack)
                                intervals
                                (iassoc intervals (- sensor-x slack) (+ sensor-x slack 1) true))))
                          empty-interval-map
                          sensors)]
    (- (count-interval-cells intervals) (count-beacons-on-row sensors y))))
;; NOT 4998562

(defn part-1
  "Solve part 1."
  ([y]
   (part-1 input y))
  ([data y]
   (count-impossible-cells-intervals (read-input (read/read-file "resources/2022/day_15.txt")) y)))


(defn part-2
  "Solve part 2."
  ([]
   (part-2 input))
  ([data]
   ))

(def sample-input
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")