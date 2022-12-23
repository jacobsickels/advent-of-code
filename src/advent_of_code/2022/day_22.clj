(ns advent-of-code.2022.day-22
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]))

(def input-map (->> (read/read-file "resources/2022/day_22_map.txt")
                    (map-indexed (fn [i line]
                                   (map-indexed
                                     (fn [j space] (list [j i] space))
                                     (str/split line #""))))
                    (apply concat)
                    (remove #(= (second %) " "))))

(def open-spaces (->> (filter #(= (second %) ".") input-map)
                      (map first)
                      set))

(def walls (->> (filter #(= (second %) "#") input-map)
                (map first)
                set))

(def input-directions (let [input-string (->> (read/read-file "resources/2022/day_22_directions.txt")
                                              first)]
                        [(utils/parse-int-col (str/split input-string #"(L|R)"))
                         (rest (str/split input-string #"\d+"))]))

(defn get-current-row-boundaries [[x y]]
  (let [row-points (->> (set/union (set (filter (fn [[x1 y1]] (= y1 y)) open-spaces))
                                   (set (filter (fn [[x1 y1]] (= y1 y)) walls)))
                        (sort-by first))]
    [(first row-points) (last row-points)]))

(defn get-current-col-boundaries [[x y]]
  (let [col-points (->> (set/union (set (filter (fn [[x1 y1]] (= x1 x)) open-spaces))
                                   (set (filter (fn [[x1 y1]] (= x1 x)) walls)))
                        (sort-by second))]
    [(first col-points) (last col-points)]))

(defn get-next-position [[x y] facing]
  (let [[[min-r-x min-r-y] [max-r-x max-r-y]] (get-current-row-boundaries [x y])
        [[min-c-x min-c-y] [max-c-x max-c-y]] (get-current-col-boundaries [x y])]
    (cond (= facing "N")
          (if (< (dec y) min-c-y)
            [x max-c-y]
            [x (dec y)])

          (= facing "E")
          (if (> (inc x) max-r-x)
            [min-r-x y]
            [(inc x) y])

          (= facing "S")
          (if (> (inc y) max-c-y)
            [x min-c-y]
            [x (inc y)])

          (= facing "W")
          (if (< (dec x) min-r-x)
            [max-r-x y]
            [(dec x) y]))))

(defn do-single-movement [curr-position facing walk-length]
  (loop [position curr-position
         to-walk walk-length]
    (cond
      (zero? to-walk)
      position

      :else (let [next-position (get-next-position position facing)]
              (cond (contains? open-spaces next-position)
                    (recur next-position (dec to-walk))

                    (contains? walls next-position)
                    position)))))

(defn get-next-facing [current-facing next-direction]
  (cond
    (= next-direction "R")
    (get {"N" "E" "E" "S" "S" "W" "W" "N"} current-facing)

    (= next-direction "L")
    (get {"N" "W" "W" "S" "S" "E" "E" "N"} current-facing)

    (nil? next-direction)
    current-facing))

(defn do-movement []
  (loop [moves (first input-directions)
         directions (second input-directions)
         direction "E"
         position [50 0]]
    (println position)
    (cond (and (empty? moves) (empty? directions))
          [(inc (first position))                           ;; my points are zero indexed so add one to these for true row/col
           (inc (second position))
           (get {"E" 0 "S" 1 "W" 2 "N" 3} direction)]

          :else (recur (rest moves)
                       (rest directions)
                       (get-next-facing direction (first directions))
                       (do-single-movement position direction (first moves))))))

(defn part-1 []
  (let [[col row facing] (do-movement)]
    (reduce + [(* 1000 row) (* 4 col) facing])))

(def all-points (set/union open-spaces walls))

(defn get-region-points [[x y] region-size]
  (let [[x1 y1] [(+ x (dec region-size)) (+ y (dec region-size))]]
    (set (filter (fn [[cx cy]] (and (<= x cx x1) (<= y cy y1))) all-points))))

;; Test data
;(def top-points (get-region-points [8 0] 4))
;(def left-points (get-region-points [4 4] 4))
;(def front-points (get-region-points [8 4] 4))
;(def bottom-points (get-region-points [8 8] 4))
;(def right-points (get-region-points [12 8] 4))
;(def back-points (get-region-points [0 4] 4))

(def top-points (get-region-points [50 50] 50))
(def left-points (get-region-points [0 100] 50))
(def front-points (get-region-points [50 100] 50))
(def bottom-points (get-region-points [0 150] 50))
(def right-points (get-region-points [100 0] 50))
(def back-points (get-region-points [50 0] 50))

(defn get-region [point]
  (cond (contains? top-points point) :top
        (contains? left-points point) :left
        (contains? front-points point) :front
        (contains? bottom-points point) :bottom
        (contains? right-points point) :right
        (contains? back-points point) :back))

(defn get-edges [points]
  (let [sorted-points (into (sorted-map) (group-by first points))]
    {"W" (sort-by second (first (vals sorted-points)))
     "S" (map #(last (sort-by second %)) (vals sorted-points))
     "E" (sort-by second (last (vals sorted-points)))
     "N" (map #(first (sort-by second %)) (vals sorted-points))}))

(def region-points {:top    top-points
                    :left   left-points
                    :front  front-points
                    :bottom bottom-points
                    :right  right-points
                    :back   back-points})

;; values are [side [region-to-match should-reverse? side-to-match new-facing]]
(def test-linked-regions {:top    {"N" [:back true "N" "S"]
                                   "E" [:right true "E" "W"]
                                   "S" [:front nil "N" "S"]
                                   "W" [:left nil "N" "S"]} ;;done

                          :left   {"N" [:top nil "W" "E"]
                                   "E" [:front nil "W" "E"]
                                   "S" [:bottom true "W" "E"]
                                   "W" [:back nil "E" "W"]} ;;done

                          :front  {"N" [:top nil "S" "N"]
                                   "E" [:right true "N" "S"]
                                   "S" [:bottom nil "N" "S"]
                                   "W" [:left nil "E" "W"]} ;;done

                          :bottom {"N" [:front nil "S" "N"]
                                   "E" [:right nil "W" "E"]
                                   "S" [:back true "S" "N"]
                                   "W" [:left true "S" "N"]} ;; done

                          :right  {"N" [:front true "E" "W"]
                                   "E" [:top true "E" "W"]
                                   "S" [:back true "W" "E"]
                                   "W" [:bottom nil "E" "W"]} ;; done

                          :back   {"N" [:top true "N" "E"]
                                   "E" [:left nil "W" "E"]
                                   "S" [:bottom true "S" "N"]
                                   "W" [:right true "S" "N"]}}) ;; done

;;      back right
;;      top
;; left front
;; bottom

;; high 182110

(def real-linked-regions {:top    {"N" [:back false "S" "N"]
                                   "E" [:right nil "S" "N"]
                                   "S" [:front nil "N" "S"]
                                   "W" [:left nil "N" "S"]} ;;done

                          :left   {"N" [:top nil "W" "E"]
                                   "E" [:front nil "W" "E"]
                                   "S" [:bottom nil "N" "S"]
                                   "W" [:back true "W" "E"]} ;;done

                          :front  {"N" [:top nil "S" "N"]
                                   "E" [:right true "E" "W"]
                                   "S" [:bottom nil "E" "W"]
                                   "W" [:left nil "E" "W"]} ;;done

                          :bottom {"N" [:left nil "S" "N"]
                                   "E" [:front nil "S" "N"]
                                   "S" [:right nil "N" "S"]
                                   "W" [:back nil "N" "S"]} ;;done

                          :right  {"N" [:bottom nil "S" "N"]
                                   "E" [:front true "E" "W"]
                                   "S" [:top nil "E" "W"]
                                   "W" [:back nil "E" "W"]} ;;done

                          :back   {"N" [:bottom nil "W" "E"]
                                   "E" [:right nil "W" "E"]
                                   "S" [:top nil "N" "S"]
                                   "W" [:left true "W" "E"]}}) ;; done

(defn get-linkages [linked-regions region]
  (loop [linkages (get linked-regions region)
         acc {}]
    (if (empty? linkages)
      acc
      (let [[dir [match-region should-reverse? take-direction new-facing]] (first linkages)
            region-edges (get-edges (region-points region))
            checking-edges (get-edges (region-points match-region))]
        (recur
          (rest linkages)
          (merge acc {dir (zipmap
                            (get region-edges dir)
                            (if should-reverse?
                              (map #(list % new-facing) (reverse (get checking-edges take-direction)))
                              (map #(list % new-facing) (get checking-edges take-direction))))}))))))

(defn get-next-cube-position [linked-regions [x y] facing]
  (let [next-pos (cond (= facing "N")
                       [x (dec y)]

                       (= facing "E")
                       [(inc x) y]

                       (= facing "S")
                       [x (inc y)]

                       (= facing "W")
                       [(dec x) y])]
    (if (contains? all-points next-pos)
      [next-pos facing]
      (let [current-region (get-region [x y])
            linkages (get-linkages linked-regions current-region)
            [new-position new-facing] (get-in linkages [facing [x y]])]
        [new-position new-facing]))))


(defn do-single-movement-cube [curr-position linked-regions facing walk-length]
  (loop [position curr-position
         face facing
         to-walk walk-length]
    (cond
      (zero? to-walk)
      [position face]

      :else (let [[next-position new-facing] (get-next-cube-position linked-regions position face)]
              (cond (contains? open-spaces next-position)
                    (recur next-position new-facing (dec to-walk))

                    (contains? walls next-position)
                    [position face])))))

(defn do-cube-movement []
  (loop [moves (first input-directions)
         directions (second input-directions)
         direction "E"
         position [50 0]]
    (cond (and (empty? moves) (empty? directions))
          [(inc (first position))                           ;; my points are zero indexed so add one to these for true row/col
           (inc (second position))
           (get {"E" 0 "S" 1 "W" 2 "N" 3} direction)]

          :else (let [[end-position end-face] (do-single-movement-cube position real-linked-regions direction (first moves))]
                  (recur (rest moves)
                         (rest directions)
                         (get-next-facing end-face (first directions))
                         end-position)))))


(defn part-2 []
  (let [[col row facing] (do-cube-movement)]
    (reduce + [(* 1000 row) (* 4 col) facing])))
