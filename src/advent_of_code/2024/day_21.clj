(ns advent-of-code.2024.day-21
  (:require [advent-of-code.shared.point :as point]
            [advent-of-code.shared.point :as points]
            [clojure.set :as set]))

(def num-to-point {0  [1 3],
                   \A [2 3],
                   7  [0 0],
                   1  [0 2],
                   4  [0 1],
                   6  [2 1],
                   3  [2 2],
                   2  [1 2],
                   9  [2 0],
                   5  [1 1],
                   8  [1 0]})

(def point-to-num {[2 2] 3, [0 0] 7, [1 0] 8, [2 3] \A, [1 1] 5, [1 3] 0, [0 2] 1, [2 0] 9, [2 1] 6, [1 2] 2, [0 1] 4})

(def arrow-to-point {\v [1 1] \< [0 1] \^ [1 0] \> [2 1] \A [2 0]})
(def point-to-arrow {[1 1] \v, [0 1] \<, [1 0] \^, [2 1] \>, [2 0] \A})

(defn next-paths [path end to-char-map]
  (if (= (last path) end)
    [path]
    (let [next-points (->> (points/cardinal-points-around (last path))
                           (filter #(get to-char-map %))
                           (remove #(contains? (set path) %)))]
      (map #(conj path %) next-points))))

(def memoized-next-paths (memoize next-paths))

(defn get-direction [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (cond
      (and (= x1 x2) (< y1 y2)) "v"
      (and (= x1 x2) (> y1 y2)) "^"
      (and (= y1 y2) (< x1 x2)) ">"
      (and (= y1 y2) (> x1 x2)) "<"
      :else nil)))

(def memoized-get-direction (memoize get-direction))

(defn point-path-to-directions [point-path]
  (str (->> (partition 2 1 point-path)
            (map (fn [[p1 p2]] (memoized-get-direction p1 p2)))
            (apply str)) "A"))

(def memoized-point-path-to-directions (memoize point-path-to-directions))

(defn bfs [start end to-point-map to-char-map]
  (loop [paths [[(get to-point-map start)]]]
    (let [last-of-paths (set (map last paths))]
      (if (and (= 1 (count last-of-paths)) (= (first last-of-paths) (get to-point-map end)))
        (->> (sort-by count paths)
             (partition-by count)
             (first)
             (map memoized-point-path-to-directions))                                       ;; get shortest paths
        (let [next-path-values (mapcat #(memoized-next-paths % (get to-point-map end) to-char-map) paths)]
          (recur (set next-path-values)))))))

(def memoized-bfs (memoize bfs))



(defn join-paths [paths-1 paths-2]
  (mapcat (fn [s1] (map (fn [s2] (str s1 s2)) paths-2)) paths-1))


;; (get-first-robot-directions [\A 0 2 9 \A])
;; => ("<A^A^>^AvvvA" "<A^A^^>AvvvA" "<A^A>^^AvvvA")

;; =============
;; Robot Layer(s)

(defn get-robot-directions
  "Should be a collection of characters like [A < v A]. Should start at A and end at A"
  [col to-point to-char]
  (->> (partition 2 1 col)
       (map (fn [[a b]] (memoized-bfs a b to-point to-char)))
       (reduce (fn [acc paths] (join-paths acc paths)) [""])
       (set)))

(defn do-arrow-robot-presses [col]
  (->> (map #(str "A" %) col)
       (mapcat #(get-robot-directions % arrow-to-point point-to-arrow))
       (sort-by count)
       (partition-by count)
       (first)))

(defn do-presses [col]
  (-> (get-robot-directions col num-to-point point-to-num)
      ;;first robot
      (do-arrow-robot-presses)
      ;; second robot
      (do-arrow-robot-presses)
      (first)
      (count)))

(def test-data [{:code [\A 0 2 9 \A] :num 29}
                {:code [\A 9 8 0 \A] :num 980}
                {:code [\A 1 7 9 \A] :num 179}
                {:code [\A 4 5 6 \A] :num 456}
                {:code [\A 3 7 9 \A] :num 379}])

(def data [{:code [\A 2 8 6 \A] :num 286}
           {:code [\A 9 7 4 \A] :num 974}
           {:code [\A 1 8 9 \A] :num 189}
           {:code [\A 8 0 2 \A] :num 802}
           {:code [\A 8 0 5 \A] :num 805}])

(defn part-1 [input]
  (->> (map #(* (do-presses (:code %)) (:num %)) input)
       (reduce +)))

(declare memoized-get-cost)

(defn get-cost [a b use-arrows? depth]
  (let [to-point (if use-arrows? arrow-to-point num-to-point)
        to-char (if use-arrows? point-to-arrow point-to-num)]
    (cond
      (zero? depth)
      (->> (memoized-bfs a b to-point to-char)
           (sort-by count)
           (first)
           (count))

      :else
      (let [ways (memoized-bfs a b to-point to-char)]
        ways
        (->> (map #(str "A" %) ways)
             (map #(->> (partition 2 1 %)
                        (map (fn [[a1 b1]] (memoized-get-cost a1 b1 true (dec depth))))
                        (reduce +)))
             (sort)
             (first))))))

(def memoized-get-cost (memoize get-cost))

(defn get-code-cost [col depth]
  (->> (partition 2 1 col)
       (map (fn [[a b]] (memoized-get-cost a b false depth)))
       (reduce +)))

(defn part-2 []
  (->> (map #(* (get-code-cost (:code %) 25) (:num %)) data)
       (reduce +)))
