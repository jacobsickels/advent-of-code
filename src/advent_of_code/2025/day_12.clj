(ns advent-of-code.2025.day-12
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def piece ["###" "##." "##."])

(defn rotate-piece [board]
  (->> (map (fn [n] (map #(nth % n) board))
            (range 0 (count (first board))))
       (map reverse)
       (map #(apply str %))
       vec))

(defn get-piece-orientations [piece]
  (let [flipped (->> (map reverse piece)
                     (map #(apply str %)))]
    #{piece
      (rotate-piece piece)
      (rotate-piece (rotate-piece piece))
      (rotate-piece (rotate-piece (rotate-piece piece)))
      flipped
      (rotate-piece flipped)
      (rotate-piece (rotate-piece flipped))
      (rotate-piece (rotate-piece (rotate-piece flipped)))}))

(defn get-points-from-piece [piece [xDelta yDelta]]
  (->> (map #(str/split % #"") piece)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x space] [space x y]) line)))
       (apply concat)
       (filter #(= (first %) "#"))
       (map (fn [[s x y]] [(+ x xDelta) (+ y yDelta)]))
       set))

(defn rect [x y]
  (combo/cartesian-product (range 0 x) (range 0 y)))

(defn get-open-points-for-piece [open-points piece]
  (loop [bps open-points
         acc []]
    (if (empty? bps)
      acc
      (let [piece-points (get-points-from-piece piece (first bps))]
        (println piece-points)
        ;; Can the piece fit in the open points
        (if (= piece-points (set/intersection open-points piece-points))
          (recur (rest bps) (conj acc [{:open-points (set/difference open-points piece-points) :piece [piece-points]}]))
          (recur (rest bps) acc))))))

(defn part-1 []
  (let [oriented-piece (first (get-piece-orientations piece))
        open-points (set (rect 4 4))] ;; size of the board
    (println open-points oriented-piece)
    (get-open-points-for-piece open-points oriented-piece)))


;; above was an attempt to do this by hand, what I read is that this is a bit of a troll
;; problem. The regions will easily fit the pieces or they wont

;; ==============================


(def tree-sizes [7 7 7 5 6 7])

(def tree-regions (->> (read/read-file "resources/2025/day_12.txt")
                       (map (fn [line]
                              (let [[area reqs] (str/split line #": ")
                                    [w h] (str/split area #"x")]
                                {:w (Integer/parseInt w)
                                 :h (Integer/parseInt h)
                                 :shapes (read-string (str "(" reqs ")"))})))))

(defn space-required [shape-requirements]
  (reduce + (map #(* %1 %2) shape-requirements tree-sizes)))

(defn can-fit? [req]
  (> (* (:w req) (:h req))
     (space-required (:shapes req))))

(defn part-1 []
  (frequencies (map can-fit? tree-regions)))