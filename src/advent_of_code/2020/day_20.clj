(ns advent-of-code.2020.day-20
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [advent-of-code.shared.point :as points]))

(def tiles (->> (read/read-file "resources/2020/day_20.txt")
                (partition-by empty?)
                (remove #(= "" (first %)))
                (map #(hash-map :id (Integer/parseInt (re-find #"\d+" (first %)))
                                :tile (rest %)))))

(defn get-edges [tile]
  {:north (first (:tile tile))
   :south (last (:tile tile))
   :west  (apply str (map first (:tile tile)))
   :east  (apply str (map last (:tile tile)))})

(defn matching-edges [edge tile]
  (let [edges (get-edges tile)]
    (->> (filter
           #(true? (or (second %) (nth % 2)))
           [[:north (= edge (:north edges)) (= edge (apply str (reverse (:north edges)))) (:id tile)]
            [:south (= edge (:south edges)) (= edge (apply str (reverse (:south edges)))) (:id tile)]
            [:west (= edge (:west edges)) (= edge (apply str (reverse (:west edges)))) (:id tile)]
            [:east (= edge (:east edges)) (= edge (apply str (reverse (:east edges)))) (:id tile)]]))))

(defn get-cardinal-tiles [tile tiles]
  (let [edges (get-edges tile)]
    {:id    (:id tile)
     :north (flatten (remove empty? (map #(matching-edges (:north edges) %) tiles)))
     :south (flatten (remove empty? (map #(matching-edges (:south edges) %) tiles)))
     :west  (flatten (remove empty? (map #(matching-edges (:west edges) %) tiles)))
     :east  (flatten (remove empty? (map #(matching-edges (:east edges) %) tiles)))}))

(defn cardinal-all [tiles]
  (map #(get-cardinal-tiles % (remove (fn [t] (= % t)) tiles)) tiles))

(defn get-corner-tiles [tile-cardinals]
  (->> (filter #(= 2 (+ (if (empty? (:north %)) 0 1)
                        (if (empty? (:east %)) 0 1)
                        (if (empty? (:south %)) 0 1)
                        (if (empty? (:west %)) 0 1)))
               tile-cardinals)))

(defn part-1 []
  (->> (cardinal-all tiles)
       (get-corner-tiles)
       (map :id)
       (reduce *)))

(defn rotate-piece [piece]
  (map (fn [i] (apply str (map (fn [row] (nth row i)) piece))) (reverse (range 0 10))))

(defn rotate-times [rotate-fn col times]
  (loop [t times
         p col]
    (if (zero? t)
      p
      (recur (dec t) (rotate-fn p)))))

(defn flip [col]
  (map #(apply str (reverse %)) col))

(def test-puzzle [[[1951 2 true] [2311 2 true] [3079 0 false]]
                  [[2729 2 true] [1427 2 true] [2473 1 true]]
                  [[2971 2 true] [1489 2 true] [1171 0 true]]])

(def puzzle [[1249 2029 2129 3413 2411 1373 1801 1039 1229 2137 3761 3169]
             [1607 1697 2293 1091 2971 3391 3079 2251 2269 2287 1321 2081]
             [2963 3923 1031 1873 3797 2887 2221 3583 1753 3917 1571 3301]
             [2707 2477 3697 3739 2143 1489 3001 2393 3313 2797 2417 1301]
             [1789 3733 1723 1163 3659 1931 3929 3907 1787 1949 3541 2689]
             [1613 3767 1013 2027 2423 1721 2333 3163 2699 3671 3049 1499]
             [3461 3209 1867 2339 1429 1879 2179 3643 1747 3299 1297 1777]
             [1151 2237 3989 1933 3257 2579 3863 1069 2381 1223 2063 3833]
             [1511 2273 3793 3847 1553 3469 1823 1997 1279 2879 2593 2459]
             [1171 2549 2311 3253 2791 3547 1783 3931 3319 3853 2383 1583]
             [1951 2731 3331 2837 3167 3251 1033 1523 3457 1213 3727 1283]
             [3467 2719 3511 1129 1097 1549 1453 1871 2161 2711 1399 1019]])

(def orientations {3797 {:rotations 2, :flip false},        ;; done
                   1097 {:rotations 1, :flip false},        ;; done
                   3671 {:rotations 1, :flip true},         ;; done
                   1553 {:rotations 0, :flip false},        ;; done
                   3251 {:rotations 1, :flip false},        ;; done
                   1171 {:rotations 3, :flip false},        ;; done
                   2393 {:rotations 1, :flip false},        ;; done
                   3547 {:rotations 2, :flip true},         ;; done
                   1777 {:rotations 2, :flip false},        ;; done
                   3767 {:rotations 2, :flip true},         ;; done
                   2269 {:rotations 2, :flip true},         ;; done
                   1933 {:rotations 1, :flip false},        ;; done
                   2081 {:rotations 0, :flip false},        ;; done
                   2971 {:rotations 2, :flip true},         ;; done
                   2549 {:rotations 1, :flip false},        ;; done
                   2129 {:rotations 1, :flip true},         ;; done
                   1871 {:rotations 1, :flip true},         ;; done
                   3847 {:rotations 0, :flip false},        ;; done
                   1753 {:rotations 2, :flip false},        ;; done
                   2237 {:rotations 0, :flip false},        ;; done
                   1949 {:rotations 3, :flip true},         ;; done
                   3457 {:rotations 2, :flip false},        ;; done
                   3863 {:rotations 2, :flip true},         ;; done
                   2179 {:rotations 1, :flip true},         ;; done
                   3049 {:rotations 3, :flip true},         ;; done
                   1997 {:rotations 3, :flip true},         ;; done
                   1039 {:rotations 0, :flip false},        ;; done
                   1783 {:rotations 2, :flip false},        ;; done
                   1697 {:rotations 0, :flip false},        ;; done
                   2797 {:rotations 2, :flip true},         ;; done
                   1721 {:rotations 3, :flip false},        ;; done
                   3391 {:rotations 0, :flip false},        ;; done
                   2707 {:rotations 1, :flip false},        ;; done
                   2333 {:rotations 2, :flip false},        ;; done
                   2311 {:rotations 0, :flip false},        ;; done
                   2339 {:rotations 1, :flip false},        ;; done
                   3257 {:rotations 1, :flip false},        ;; done
                   1607 {:rotations 3, :flip false},        ;; done
                   2731 {:rotations 1, :flip true},         ;; done
                   2719 {:rotations 2, :flip true},         ;; done
                   2287 {:rotations 3, :flip true},         ;; done
                   1019 {:rotations 2, :flip false},        ;; done
                   1549 {:rotations 3, :flip false},        ;; done
                   1867 {:rotations 3, :flip false},        ;; done
                   3301 {:rotations 3, :flip true},         ;; done
                   3697 {:rotations 3, :flip false},        ;; done
                   1033 {:rotations 1, :flip false},        ;; done
                   2029 {:rotations 3, :flip false},        ;; done
                   2221 {:rotations 2, :flip false},        ;; done
                   1091 {:rotations 0, :flip false},        ;; done
                   3169 {:rotations 3, :flip true},         ;; done
                   1523 {:rotations 1, :flip true},         ;; done
                   3541 {:rotations 1, :flip false},        ;; done
                   3511 {:rotations 3, :flip false},        ;; done
                   1129 {:rotations 3, :flip true},         ;; done
                   2063 {:rotations 1, :flip false},        ;; done
                   1613 {:rotations 3, :flip true},         ;; done
                   1873 {:rotations 1, :flip false},        ;; done
                   3167 {:rotations 3, :flip false},        ;; done
                   1879 {:rotations 1, :flip false},        ;; done
                   2579 {:rotations 1, :flip true},         ;; done
                   1223 {:rotations 2, :flip false},        ;; done
                   1801 {:rotations 2, :flip true},         ;; done
                   3733 {:rotations 3, :flip true},         ;; done
                   2593 {:rotations 1, :flip true},         ;; done
                   3739 {:rotations 3, :flip true},         ;; done
                   1571 {:rotations 0, :flip false},        ;; done
                   1229 {:rotations 2, :flip true},         ;; done
                   2689 {:rotations 1, :flip false},        ;; done
                   1489 {:rotations 0, :flip false},        ;; done
                   1823 {:rotations 2, :flip false},        ;; done
                   3727 {:rotations 3, :flip false},        ;; done
                   3467 {:rotations 0, :flip false},        ;; done
                   2791 {:rotations 2, :flip false},        ;; done
                   2417 {:rotations 2, :flip false},        ;; done
                   2837 {:rotations 1, :flip false},        ;; done
                   2137 {:rotations 3, :flip true},         ;; done
                   2423 {:rotations 2, :flip false},        ;; done
                   3907 {:rotations 2, :flip true},         ;; done
                   2251 {:rotations 1, :flip false},        ;; done
                   3583 {:rotations 1, :flip false},        ;; done
                   2699 {:rotations 2, :flip false},        ;; done
                   1321 {:rotations 3, :flip false},        ;; done
                   3299 {:rotations 2, :flip true},         ;; done
                   1213 {:rotations 1, :flip false},        ;; done
                   3209 {:rotations 3, :flip false},        ;; done
                   3659 {:rotations 1, :flip true},         ;; done
                   3469 {:rotations 2, :flip true},         ;; done
                   1429 {:rotations 3, :flip false},        ;; done
                   3319 {:rotations 1, :flip false},        ;; done
                   2027 {:rotations 2, :flip true},         ;; done
                   3761 {:rotations 0, :flip false},        ;; done
                   1069 {:rotations 0, :flip false},        ;; done
                   3929 {:rotations 1, :flip false},        ;; done
                   2887 {:rotations 3, :flip false},        ;; done
                   1163 {:rotations 0, :flip false},        ;; done
                   3253 {:rotations 3, :flip true},         ;; done
                   2143 {:rotations 2, :flip true},         ;; done
                   2161 {:rotations 3, :flip false},        ;; done
                   1931 {:rotations 2, :flip true},         ;; done
                   3853 {:rotations 2, :flip true},         ;; done
                   1399 {:rotations 3, :flip true},         ;; done
                   1789 {:rotations 2, :flip true},         ;; done
                   2711 {:rotations 3, :flip false},        ;; done
                   3833 {:rotations 3, :flip false},        ;; done
                   1723 {:rotations 1, :flip true},         ;; done
                   2273 {:rotations 0, :flip false},        ;; done
                   3923 {:rotations 3, :flip false},        ;; done
                   1031 {:rotations 2, :flip true},         ;; done
                   1499 {:rotations 3, :flip true},         ;; done
                   1013 {:rotations 3, :flip true},         ;; done
                   3931 {:rotations 0, :flip false},        ;; done
                   1249 {:rotations 1, :flip false},        ;; done
                   1747 {:rotations 2, :flip true},         ;; done
                   3079 {:rotations 0, :flip false},        ;; done
                   3989 {:rotations 1, :flip true},         ;; done
                   1373 {:rotations 3, :flip true},         ;; done
                   3163 {:rotations 2, :flip true},         ;; done
                   2963 {:rotations 2, :flip false},        ;; done
                   1583 {:rotations 1, :flip false},        ;; done
                   1787 {:rotations 3, :flip true},         ;; done
                   3461 {:rotations 3, :flip false},        ;; done
                   1511 {:rotations 3, :flip false},        ;; done
                   2293 {:rotations 2, :flip false},        ;; done
                   2459 {:rotations 0, :flip false},        ;; done
                   3793 {:rotations 1, :flip true},         ;; done
                   2383 {:rotations 3, :flip false},        ;; done
                   1279 {:rotations 2, :flip false},        ;; done
                   1297 {:rotations 0, :flip false},        ;; done
                   1151 {:rotations 3, :flip false},        ;; done
                   2381 {:rotations 2, :flip true},         ;; done
                   3331 {:rotations 3, :flip false},        ;; done
                   1301 {:rotations 3, :flip false},        ;; done
                   1283 {:rotations 1, :flip false},        ;; done
                   3917 {:rotations 2, :flip false},        ;; done
                   3643 {:rotations 1, :flip true},         ;; done
                   1951 {:rotations 1, :flip true},         ;; done
                   2879 {:rotations 1, :flip false},        ;; done
                   2477 {:rotations 2, :flip true},         ;; done
                   3313 {:rotations 3, :flip false},        ;; done
                   3413 {:rotations 1, :flip true},         ;; done
                   1453 {:rotations 1, :flip false},        ;; done
                   3001 {:rotations 1, :flip true},         ;; done
                   2411 {:rotations 0, :flip false}})       ;; done

(def empty-tile ["          "
                 "          "
                 "          "
                 "          "
                 "          "
                 "          "
                 "          "
                 "          "
                 "          "
                 "          "])

(defn nice-get-around [tile-id]
  (let [indexed-puzzle (->> (map-indexed (fn [i row] (map-indexed (fn [j col] (list [j i] col)) row)) puzzle)
                            (apply concat))
        get-pnt (fn [pnt] (first (filter #(= (first %) pnt) indexed-puzzle)))
        [x y] (->> indexed-puzzle
                   (filter #(= (second %) tile-id))
                   (ffirst))]
    {:north (second (get-pnt [x (dec y)]))
     :south (second (get-pnt [x (inc y)]))
     :east  (second (get-pnt [(inc x) y]))
     :west  (second (get-pnt [(dec x) y]))}))


(defn get-cardinal [tile-id tiles]
  (first (filter #(= (:id %) tile-id) tiles)))

(defn piece [tile-id]
  (:tile (first (filter #(= (:id %) tile-id) tiles))))


(defn cardinal-orientations [tiles]
  (map #(get-cardinal-tiles % (remove (fn [t] (= % t)) tiles)) tiles))

(defn orient-piece [tile-id]
  (let [rotations (:rotations (get orientations tile-id))
        flip? (:flip (get orientations tile-id))
        rotated (rotate-times rotate-piece (piece tile-id) (or rotations 0))]
    (if flip? (flip rotated) rotated)))

(defn assoc-truthy [map key val]
  (if (some? key)
    (assoc map key val)
    map))

(defn get-pieces-around [tile-id]
  (let [cardinal (get-cardinal tile-id (cardinal-all tiles))]
    (-> (assoc-truthy {} tile-id (orient-piece (:id cardinal)))
        (assoc-truthy (last (:north cardinal)) (orient-piece (last (:north cardinal))))
        (assoc-truthy (last (:east cardinal)) (orient-piece (last (:east cardinal))))
        (assoc-truthy (last (:south cardinal)) (orient-piece (last (:south cardinal))))
        (assoc-truthy (last (:west cardinal)) (orient-piece (last (:west cardinal)))))))

(defn get-missing [tile-id]
  (let [already-found (set (flatten puzzle))
        cardinal (get-cardinal tile-id (cardinal-all tiles))]
    (set/difference (set (list (last (flatten (:north cardinal)))
                               (last (flatten (:east cardinal)))
                               (last (flatten (:south cardinal)))
                               (last (flatten (:west cardinal)))))
                    already-found)))


(defn get-cardinal [tile-id tiles]
  (first (filter #(= (:id %) tile-id) tiles)))

(defn merge-row [row]
  (map #(->> (map (fn [r] (nth r %)) row)
             (apply str))
       (range 0 10)))

(defn merge-row-no-edges [row]
  (map #(->> (map (fn [r] (nth r %)) row)
             (apply str))
       (range 0 8)))


(defn pretty-print [self]
  (let [around (nice-get-around self)]
    (println around)
    (->> (map merge-row (list [empty-tile (or (orient-piece (:north around)) empty-tile) empty-tile]
                              [(or (orient-piece (:west around)) empty-tile) (orient-piece self) (or (orient-piece (:east around)) empty-tile)]
                              [empty-tile (or (orient-piece (:south around)) empty-tile) empty-tile])))))


(defn remove-edges [tile]
  (->> (drop 1 tile)
       (drop-last)
       (map #(drop 1 %))
       (map drop-last)
       (map #(apply str %))))



(defn orient-pieces-view [puzzle]
  (->> (map #(map (fn [[tile-id rotations flip?]]
                    (let [rotated (rotate-times rotate-piece (piece tile-id) (or rotations 0))]
                      (if flip? (flip rotated) rotated))) %) puzzle)
       (map merge-row)))

(defn orient-pieces [puzzle]
  (->> (map #(map (fn [[tile-id rotations flip?]]
                    (let [rotated (rotate-times rotate-piece (piece tile-id) rotations)]
                      (if flip? (flip rotated) rotated))) %) puzzle)))


(defn put-puzzle-together [puzzle]
  (->> (map #(map remove-edges %) (map #(map orient-piece %) puzzle))
       (map merge-row-no-edges)
       (flatten)))


(def monster ["                  # "
              "#    ##    ##    ###"
              " #  #  #  #  #  #   "])

(def monster-points (->> (map-indexed (fn [i row] (map-indexed (fn [j col] (list [j i] col)) row)) monster)
                         (apply concat)
                         (filter #(= (second %) \#))
                         (map first)
                         set))

(defn puzzle-val [[x y] together-puzzle]
  (nth (nth together-puzzle y nil) x nil))

(defn transform-monster-points [[x y]]
  (map (fn [[mx my]] [(+ mx x) (+ my y)]) monster-points))

(defn is-monster-at-points? [[x y] together-puzzle]
  (let [m-points (transform-monster-points [x y])
        check-points (map #(puzzle-val % together-puzzle) m-points)
        freq (frequencies check-points)]
    [(and (= 1 (count freq)) (= \# (ffirst freq))) m-points]))

(defn rotate-together-puzzle [together-puzzle]
  (map (fn [i] (apply str (map (fn [row] (nth row i)) together-puzzle))) (reverse (range 0 (count together-puzzle)))))

(defn get-roughness [together-puzzle]
  (let [points (combo/cartesian-product (range 0 (count together-puzzle)) (range 0 (count (first together-puzzle))))
        m-occupied-points (->> (map #(let [[monster? m-points] (is-monster-at-points? % together-puzzle)]
                                       [monster? m-points])
                                    points)
                               (filter #(true? (first %)))
                               (mapcat second)
                               set)
        wave-points (->> (map #(list % (puzzle-val % together-puzzle)) points)
                         (filter #(= (second %) \#))
                         (map first))]
    (count (set/difference (set wave-points) m-occupied-points))))

(defn all-orientation-roughness [puzzle]
  (let [together-puzzle (put-puzzle-together puzzle)]
    (list (get-roughness together-puzzle)
          (get-roughness (rotate-times rotate-together-puzzle together-puzzle 1))
          (get-roughness (rotate-times rotate-together-puzzle together-puzzle 2))
          (get-roughness (rotate-times rotate-together-puzzle together-puzzle 3))
          (get-roughness (flip together-puzzle))
          (get-roughness (flip (rotate-times rotate-together-puzzle together-puzzle 1)))
          (get-roughness (flip (rotate-times rotate-together-puzzle together-puzzle 2)))
          (get-roughness (flip (rotate-times rotate-together-puzzle together-puzzle 3))))))
