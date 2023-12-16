(ns advent-of-code.2023.day-16
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))


(def data (read/read-file "resources/2023/day_16.txt"))

(def mirrors (->> data
                  (map-indexed (fn [row line]
                                 (map-indexed (fn [column character]
                                                (if (contains? #{\\ \/} character)
                                                  [column row character]
                                                  nil))
                                              line)))
                  (apply concat)
                  (remove nil?)))

(def splitters (->> data
                    (map-indexed (fn [row line]
                                   (map-indexed (fn [column character]
                                                  (if (contains? #{\| \-} character)
                                                    [column row character]
                                                    nil))
                                                line)))
                    (apply concat)
                    (remove nil?)))

(defn border-points [width height]
  (->> (combo/cartesian-product (range -1 (inc width)) (range -1 (inc height)))
       (filter (fn [[x y]] (or (neg? x) (neg? y) (= x width) (= y height))))
       (map vec)
       set))

(def border (border-points (count (first data)) (count data)))

(defn get-next-point-from-direction [direction [x y]]
  (cond
    (= direction :right) [(inc x) y direction]
    (= direction :left) [(dec x) y direction]
    (= direction :up) [x (dec y) direction]
    (= direction :down) [x (inc y) direction]))

(defn is-mirror? [[x y]]
  (let [mirror-point (first (filter (fn [[mx my]] (and (= mx x) (= my y))) mirrors))]
    (if (nil? mirror-point)
      [false nil]
      [true mirror-point])))

(defn is-splitter? [[x y]]
  (let [splitter-point (first (filter (fn [[sx sy]] (and (= sx x) (= sy y))) splitters))]
    (if (nil? splitter-point)
      [false nil]
      [true splitter-point])))

(defn get-next-mirror-point [mirror direction]
  (let [[x y mc] mirror]
    (cond
      (and (= mc \\) (= direction :right)) [x (inc y) :down]
      (and (= mc \\) (= direction :left)) [x (dec y) :up]
      (and (= mc \\) (= direction :up)) [(dec x) y :left]
      (and (= mc \\) (= direction :down)) [(inc x) y :right]

      (and (= mc \/) (= direction :right)) [x (dec y) :up]
      (and (= mc \/) (= direction :left)) [x (inc y) :down]
      (and (= mc \/) (= direction :up)) [(inc x) y :right]
      (and (= mc \/) (= direction :down)) [(dec x) y :left])))

(defn get-next-splitter-points [splitter direction]
  (let [[x y sc] splitter]
    (cond
      (and (= sc \|) (contains? #{:up :down} direction))
      [(get-next-point-from-direction direction [x y])]

      (and (= sc \-) (contains? #{:right :left} direction))
      [(get-next-point-from-direction direction [x y])]

      (= sc \|)
      [[x (inc y) :down] [x (dec y) :up]]

      (= sc \-)
      [[(inc x) y :right] [(dec x) y :left]])))

(def memoized-get-next-mirror-point (memoize get-next-mirror-point))
(def memoized-get-next-splitter-points (memoize get-next-splitter-points))
(def memoized-is-mirror? (memoize is-mirror?))
(def memoized-is-splitter? (memoize is-splitter?))

(defn remove-border-points [next-light-points]
  (remove (fn [[x y]] (contains? border [x y])) next-light-points))

(defn remove-already-lighted-points [visited next-light-points]
  (remove (fn [light] (contains? visited light)) next-light-points))

(defn get-next-light [visited-points light-points]
  (->> (mapcat (fn [[x y direction]]
                 (let [[is-point-mirror? mirror] (memoized-is-mirror? [x y])
                       [is-point-splitter? splitter] (memoized-is-splitter? [x y])]
                   (cond
                     is-point-mirror?
                     [(memoized-get-next-mirror-point mirror direction)]

                     is-point-splitter?
                     (memoized-get-next-splitter-points splitter direction)

                     :else [(get-next-point-from-direction direction [x y])])))
               light-points)
       #_(apply concat)
       (remove-border-points)
       (remove-already-lighted-points visited-points)
       vec))

;; This is incredibly slow, maybe a different data structure for storing light-points would be better
;(defn find-energized-state [starting]
;  (println starting)
;  (loop [light [[starting]]]
;    (let [next-light (get-next-light (set (apply concat (butlast light))) (last light))]
;      (if (= (set (apply concat light)) (set (apply concat (conj light next-light))))
;        (count (set (map (fn [[x y]] [x y]) (apply concat light))))
;        (recur (conj light next-light))))))

(defn find-energized-state-2 [starting]
  (loop [light #{starting}
         next-light-points [starting]]
    (let [next-light (get-next-light light next-light-points)
          full-lights (set/union light (set next-light))]
      (if (= light full-lights)
        (count (set (map (fn [[x y]] [x y]) light)))
        (recur full-lights next-light)))))


;(defn part-1 []
;  (time (find-energized-state [0 0 :right])))

(defn part-1 []
  (time (find-energized-state-2 [0 0 :right])))

(defn part-2 []
  (let [width (count (first data))
        height (count data)
        check-points (->> (combo/cartesian-product (range 0 width) (range 0 height))
                          (filter (fn [[x y]] (or (zero? x) (zero? y) (= x (dec width)) (= y (dec height)))))
                          (mapcat (fn [[x y]]
                                    (cond
                                      (and (zero? x) (zero? y)) [[x y :right] [x y :down]]
                                      (and (zero? x) (= y (dec height))) [[x y :right] [x y :up]]
                                      (and (zero? y) (= x (dec width))) [[x y :left] [x y :down]]
                                      (and (= x (dec width)) (= y (dec height))) [[x y :left] [x y :up]]

                                      (zero? x) [[x y :right]]
                                      (zero? y) [[x y :down]]
                                      (= x (dec width)) [[x y :left]]
                                      (= y (dec height)) [[x y :up]]))))]
    (->> (map find-energized-state-2 check-points)
         sort
         last)))
