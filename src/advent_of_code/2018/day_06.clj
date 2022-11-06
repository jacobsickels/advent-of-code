(ns advent-of-code.2018.day-06
  (:require [clojure.math.combinatorics :as combo]))

(def test-data [[1 1]
                [1 6]
                [8 3]
                [3 4]
                [5 5]
                [8 9]])

(def data ['(292 73)
           '(204 176)
           '(106 197)
           '(155 265)
           '(195 59)
           '(185 136)
           '(54 82)
           '(209 149)
           '(298 209)
           '(274 157)
           '(349 196)
           '(168 353)
           '(193 129)
           '(94 137)
           '(177 143)
           '(196 357)
           '(272 312)
           '(351 340)
           '(253 115)
           '(109 183)
           '(252 232)
           '(193 258)
           '(242 151)
           '(220 345)
           '(336 348)
           '(196 203)
           '(122 245)
           '(265 189)
           '(124 57)
           '(276 204)
           '(309 125)
           '(46 324)
           '(345 228)
           '(251 134)
           '(231 117)
           '(88 112)
           '(256 229)
           '(49 201)
           '(142 108)
           '(150 337)
           '(134 109)
           '(288 67)
           '(297 231)
           '(310 131)
           '(208 255)
           '(246 132)
           '(232 45)
           '(356 93)
           '(356 207)
           '(83 97)])

(defn- find-bounds [col]
  (let [x-points (sort-by first col)
        y-points (sort-by second col)]
    {:x-low  (ffirst x-points)
     :x-high (first (last x-points))
     :y-low  (last (first y-points))
     :y-high (last (last y-points))}))


(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn- point-belongs-to [[x y] col]
  (let [distance-to-points (->> (map (fn [point] (vector (manhattan-distance [x y] point) point)) col)
                                (sort-by first))
        smallest-distance (ffirst distance-to-points)
        points-belongs-to (filter #(= smallest-distance (first %)) distance-to-points)]
    (if (> (count points-belongs-to) 1)
      :multiple
      (second (first points-belongs-to)))))

(defn- determine-finite [col]
  (let [{:keys [x-low x-high y-low y-high]} (find-bounds col)
        width (range 0 (inc x-high))
        height (range 0 (inc y-high))
        field (combo/cartesian-product width height)
        points (filter (fn [[x y]]
                         (or (contains? #{x-low x-high} x)
                             (contains? #{y-low y-high} y)))
                       field)]
    (disj (->> (map (fn [point] (point-belongs-to point col)) points)
               set)
          :multiple)))

(defn get-infinite-points [col]
  (let [determined (determine-finite col)]
    determined))

(defn part-1 [w h col]
  (let [infinite-points (get-infinite-points col)
        field (combo/cartesian-product (range 0 w) (range 0 h))
        initial-data (reduce (fn [acc point]
                               (if (contains? infinite-points point)
                                 (assoc acc point :infinite)
                                 (assoc acc point 0)))
                             {}
                             col)]
    (reduce (fn [acc point]
              (let [belongs-to (point-belongs-to point col)]
                (if (or (= :multiple belongs-to)
                        (= :infinite (get acc belongs-to)))
                  acc
                  (assoc acc belongs-to (inc (get acc belongs-to))))))
            initial-data
            field)))

(defn- manhattan-to-all-points [[x y] col]
  (map (fn [point] (manhattan-distance [x y] point)) col))

(defn- manhattan-all-within [point col within]
  (< (reduce + (manhattan-to-all-points point col)) within))

(defn part-2 [w h col within]
  (let [field (combo/cartesian-product (range 0 w) (range 0 h))]
    (filter (fn [point] (manhattan-all-within point col within)) field)))



