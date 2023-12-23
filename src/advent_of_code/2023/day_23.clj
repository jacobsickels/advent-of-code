(ns advent-of-code.2023.day-23
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (read/read-file "resources/2023/day_23.txt"))

(def starting-point (let [[y col] (->> (map-indexed (fn [i col] (if (str/includes? col "S") [i col] [i nil])) data)
                                       (filter #(not (nil? (second %))))
                                       first)]
                      [(str/index-of col "S") y]))

(def finish-point (let [[y col] (->> (map-indexed (fn [i col] (if (str/includes? col "F") [i col] [i nil])) data)
                                     (filter #(not (nil? (second %))))
                                     first)]
                    [(str/index-of col "F") y]))

(defn is-wall? [point]
  (= \# (get-in data (reverse point))))

(defn is-slope? [point slope-type]
  (= slope-type (get-in data (reverse point))))

(defn get-valid-points-around [[x y]]
  (->> [(when (not (is-slope? [(dec x) y] \>)) [(dec x) y])
        (when (not (is-slope? [(inc x) y] \<)) [(inc x) y])
        (when (not (is-slope? [x (dec y)] \v)) [x (dec y)])
        (when (not (is-slope? [x (inc y)] \^)) [x (inc y)])]
       (remove nil?)
       (remove is-wall?)))


(defn get-valid-next-points [[x y] visited]
  (let [point-character (get-in data (reverse [x y]))
        valid-points-around (get-valid-points-around [x y])]
    (cond
      ;; only valid next point is down
      (= \v point-character) [[x (inc y)]]

      ;; only valid next point is right
      (= \> point-character) [[(inc x) y]]

      ;; only valid next point is left
      (= \< point-character) [[(dec x) y]]

      ;; only valid next point is up
      (= \^ point-character) [[x (dec y)]]

      :else (set/difference (set valid-points-around) visited))))

(defn get-next-paths [paths get-valid-next-points-fn]
  (reduce (fn [acc path]
            (if (= (last path) finish-point)
              (conj acc path)
              (let [next-points (get-valid-next-points-fn (last path) (set path))]
                (cond
                  (< 1 (count next-points))
                  (concat acc (mapv #(conj (vec path) %) next-points))

                  (empty? next-points)
                  acc

                  :else (conj acc (concat path next-points))))))
          []
          paths))

(defn part-1 []
  (loop [paths [[[1 -1] starting-point]]]
    (if (= #{finish-point} (set (map last paths)))
      (->> (map count paths)
           (map #(- % 2))
           (apply max))
      (let [next-paths (get-next-paths paths get-valid-next-points)]
        (recur next-paths)))))

(defn get-valid-points-around-2 [[x y]]
  (->> (points/cardinal-points-around [x y])
       (remove is-wall?)
       (remove (fn [[x y]] (or (neg? y) (>= y (count data)))))))

(def memoized-get-valid-points-around (memoize get-valid-points-around-2))

(defn get-valid-next-points-2 [[x y] visited-intersections]
  (let [valid-points-around (memoized-get-valid-points-around [x y])]
    (set/difference (set valid-points-around) visited-intersections)))

(defn get-next-paths-2 [paths]
  (reduce (fn [acc path]
            (if (= (last (:path path)) finish-point)
              (conj acc path)
              (let [next-points (get-valid-next-points-2 (last (:path path)) (conj (:visited-intersections path) (first (:path path))))]
                (cond
                  (< 1 (count next-points))
                  (concat acc (mapv (fn [p]
                                      (let [next-path (conj (:path path) p)
                                            next-path (if (= 3 (count next-path)) (vec (drop 1 next-path)) next-path)]
                                        {:path                  next-path
                                         :steps                 (inc (:steps path))
                                         :visited-intersections (conj (:visited-intersections path) (last (:path path)))}))
                                    next-points))

                  (empty? next-points)
                  acc

                  :else (let [next-path (conj (:path path) (first next-points))
                              next-path (if (= 3 (count next-path)) (vec (drop 1 next-path)) next-path)]
                          (conj acc (assoc path :path next-path :steps (inc (:steps path)))))))))
          []
          paths))

;; Need to make a graph with S -> intersections -> F
;; bfs on the nodes rather than counting steps, it takes too long

;; 5000 < a < 15000
;(defn part-2 []
;  (let [starting-path {:path [starting-point] :visited-intersections #{[1 -1]} :steps 0}]
;    (loop [paths [starting-path]
;           iterations 0]
;      (println iterations)
;      (if (= iterations 15000)
;        paths
;        (if (= #{finish-point} (set (->> (map :path paths) (map last))))
;          (->> (map :steps paths)
;               (apply max))
;          (let [next-paths (get-next-paths-2 paths)]
;            (recur next-paths (inc iterations))))))))

(defn is-intersection? [point]
  (cond
    (= starting-point point) false
    (= finish-point point) false
    :else (let [points-around (points/cardinal-points-around point)]
            (->> (remove is-wall? points-around)
                 count
                 (<= 3)))))

(def intersections (let [width (count (first data))
                         height (count data)
                         points (combo/cartesian-product (range 0 width) (range 0 height))]
                     (->> (remove is-wall? points)
                          (remove #(not (is-intersection? %)))
                          set)))

(defn get-next-step-point [[x y] visited]
  (let [valid-points-around (memoized-get-valid-points-around [x y])]
    (set/difference (set valid-points-around) visited)))

(defn steps-to-next-intersection [point intersection]
  (loop [p point
         visited #{intersection}
         steps 1]
    (let [next-point (first (get-next-step-point p visited))]
      (cond
        (= finish-point next-point)
        {next-point (inc steps)}

        (= starting-point next-point)
        {next-point (inc steps)}

        (contains? intersections next-point)
        {next-point (inc steps)}

        :else (recur next-point (conj visited p) (inc steps))))))

(defn point-graph [point]
  (let [valid-points-around (memoized-get-valid-points-around point)]
    (->> (map #(steps-to-next-intersection % point) valid-points-around)
         (apply merge))))

(defn make-step-graph []
  (loop [points-to-check [starting-point]
         graph {}]
    (if (empty? points-to-check)
      graph
      (let [destinations (point-graph (first points-to-check))
            next-points-to-check (set/difference (set (keys destinations)) (set (keys graph)))]
        (recur (set (rest (concat points-to-check next-points-to-check)))
               (assoc graph (first points-to-check) destinations))))))

(def step-graph (make-step-graph))

(defn graph-get-next-paths [path]
  (if (= finish-point (:point path))
    [path]
    (let [next-path-points (->> (get step-graph (:point path))
                                (keys))
          next-path-points (set/difference (set next-path-points) (:visited path))]
      (->> next-path-points
           (map (fn [p] {:point   p
                         :visited (conj (:visited path) p)
                         :steps   (+ (:steps path) (get-in step-graph [(:point path) p]))}))))))

(defn part-2 []
  (loop [paths [{:point starting-point :steps 0 :visited #{starting-point}}]
         iterations 0]
    (if (= #{finish-point} (set (map :point paths)))
      (->> (map :steps paths)
           (apply max))
      (recur (mapcat graph-get-next-paths paths)
             (inc iterations)))))
