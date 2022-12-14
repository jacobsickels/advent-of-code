(ns advent-of-code.2021.day-20
  (:require [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

;; As a note, this code works for input data and not for example data
;; This is because the first character in my input data is a # which will
;; Switch the border between (# and .) The example data doesn't check for this

(def image (read/read-file "resources/day_20.txt"))
(def enhancement (read/load-edn "day_20_enhancement.edn"))

(def image_p (map #(map str %) (read/load-edn "day_20.edn")))
(def enhancement_p (read/load-edn "day_20_enhancement_p.edn"))

;(defn get-image-pixels [enhancement iteration image]
;  (let [width  (count (first image))
;        height (count image)
;        points (core/point-grid -1 width -1 height)]
;    (sort-by
;      (fn [[point pl]] (-> point second))
;      (map
;        (fn [point-list]
;          [(nth point-list (quot (count point-list) 2))
;           (map
;             (fn [[x y]]
;               (cond
;                 (or (neg? y) (neg? x) (>= y height) (>= x width))
;                 (if
;                   (and (even? iteration) (= (first enhancement) \#))
;                   "."
;                   "#")
;                 :else (str (nth (nth image y) x))))
;             point-list)])
;        (map core/points-around-inclusive points)))))

;(defn image-pixels-enhanced [enhancement image-pixels]
;  (map
;    (fn [[point pixel-list]]
;      (let [index (Long/parseLong (apply str (replace {"#" 1 "." 0} pixel-list))
;                                  2)]
;        [point (get enhancement index)]))
;    image-pixels))
;
;(defn picture-iteration [image enhancement iteration]
;  (->> image
;       (get-image-pixels enhancement iteration)
;       (image-pixels-enhanced enhancement)
;       (partition-by (fn [[point pixel]] (second point)))
;       (map (fn [pixel-list] (apply str (map second pixel-list))))))

;(defn part-1 [image enhancement]
;  (reduce (fn [acc l] (+ acc (get (frequencies l) \#))) 0
;          (-> (picture-iteration image enhancement 0)
;              (picture-iteration enhancement 1))))

(defn image-pixels [iteration enhancement image]
  (let [width  (count (first image))
        height (count image)
        points (core/point-grid -2 (+ 1 width) -2 (+ 1 height))]
    (->> (map core/points-around-inclusive points)
         (map (fn [point-list]
                [(nth point-list (quot (count point-list) 2))
                 (map
                   (fn [[x y]]
                     (cond
                       (or (neg? y) (neg? x) (>= y height) (>= x width))
                       (if (odd? iteration) "#" ".")

                       :else (str (nth (nth image y) x))))
                   point-list)])))))

(defn image-pixels-enhanced [enhancement image-pixels]
  (map
    (fn [[point pixel-list]]
      (let [index (Long/parseLong (apply str (replace {"#" 1 "." 0} pixel-list)) 2)]
        [point (get enhancement index)]))
    image-pixels))

(defn drop-border [image-pixels]
  (->>
    image-pixels
    butlast
    rest
    (map #(-> %
              butlast
              rest))))

(defn picture-iteration [image enhancement iteration]
  (->> image
       (image-pixels iteration enhancement)
       (image-pixels-enhanced enhancement)
       (sort-by (fn [[point pixel]] (second point)))
       (partition-by (fn [[point pixel]] (second point)))
       drop-border
       (map (fn [pixel-list] (apply str (map second pixel-list))))))

(defn part-1 [image enhancement]
  (reduce (fn [acc l] (+ acc (or (get (frequencies l) \#) 0)))
          0
          (-> (picture-iteration image enhancement 0)
              (picture-iteration enhancement 1))))

(defn part-2 [image enhancement]
  (loop [img image
         step 0]
    (if (= step 50)
      (reduce (fn [acc l] (+ acc (or (get (frequencies l) \#) 0))) 0 img)
      (recur (picture-iteration img enhancement step) (inc step)))))
      
         
    