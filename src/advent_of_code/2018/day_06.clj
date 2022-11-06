(ns advent-of-code.2018.day-06)

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
    {:x-low (ffirst x-points)
     :x-high (first (last x-points))
     :y-low (last (first y-points))
     :y-high (last (last y-points))}))

(defn- determine-finite [col]
  (let [{:keys [x-low x-high y-low y-high]} (find-bounds col)]
    (map
      (fn [[x y]]
        {:point [x y]
         :is-infinite? (cond
                         (>= x-low x) true
                         (<= x-high x) true
                         (>= y-low y) true
                         (<= y-high y) true
                         :else false)})
      col)))