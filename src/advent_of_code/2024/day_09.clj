(ns advent-of-code.2024.day-09
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_09.txt")
               first
               (map #(Integer/parseInt (str %)))))

(def test-data (map #(Integer/parseInt (str %)) "2333133121414131402"))

(defn do-swaps [expanded-disk]
  (loop [dot-pointer 0
         num-pointer (dec (count expanded-disk))
         acc (vec expanded-disk)]
    (if (= dot-pointer num-pointer)
      acc
      (cond
        (not (nil? (nth acc dot-pointer)))
        (recur (inc dot-pointer) num-pointer acc)

        (nil? (nth acc num-pointer))
        (recur dot-pointer (dec num-pointer) acc)

        :else
        (recur (inc dot-pointer) (dec num-pointer)
               (assoc acc dot-pointer (nth acc num-pointer) num-pointer (nth acc dot-pointer)))))))

(defn expand-disk [input]
  (loop [col input
         id 0
         on-spaces? false
         acc []]
    (if (empty? col)
      acc
      (if on-spaces?
        (recur (rest col) id false (conj acc (repeat (first col) nil)))
        (recur (rest col) (inc id) true (conj acc (repeat (first col) id)))))))

(defn part-1 [input]
  (->> (expand-disk input)
       flatten
       do-swaps
       (remove nil?)
       (map-indexed (fn [idx n] (* idx n)))
       (reduce +)))

(defn swap-file-and-spaces [file spaces]
  [file (repeat (- (count spaces) (count file)) nil)])

(defn list-insert [lst elem index]
  (let [[l r] (split-at index lst)]
    (if (nil? (first r))
      (concat l (concat [elem] (take (count elem) r)) (drop (count elem) r))
      (concat l [elem] r))))

(defn can-move-up? [acc file]
  (let [spaces (filter #(nil? (first %)))]
    (->> (map count spaces)
         (filter #(>= % (count file))))))

(defn do-swaps-by-length [expanded-disk]
  (loop [space-pointer 0
         file-pointer (dec (count expanded-disk))
         acc (vec expanded-disk)]
    (if (zero? file-pointer)
      acc
      (cond
        (= space-pointer file-pointer)
        (recur 0 (dec file-pointer) acc)

        (nil? (first (nth acc file-pointer)))
        (recur space-pointer (dec file-pointer) acc)

        (not (nil? (first (nth acc space-pointer))))
        (recur (inc space-pointer) file-pointer acc)

        ;; do swap
        (>= (count (nth acc space-pointer)) (count (nth acc file-pointer)))
        (let [[new-file left-over-spaces] (swap-file-and-spaces (nth acc file-pointer) (nth acc space-pointer))
              new-acc (if (empty? left-over-spaces)
                        (-> (assoc acc space-pointer new-file)
                            (assoc file-pointer (repeat (count new-file) nil)))
                        (-> (assoc acc space-pointer new-file)
                            (assoc file-pointer (repeat (count new-file) nil))
                            (list-insert left-over-spaces (inc space-pointer))))]
          (recur 0 (dec file-pointer) (vec new-acc)))

        :else
        (recur (inc space-pointer) file-pointer acc)))))

(defn expand-disk-2 [input]
  (loop [col input
         id 0
         on-spaces? false
         acc []]
    (if (empty? col)
      (remove empty? acc)
      (if on-spaces?
        (recur (rest col) id false (conj acc (repeat (first col) nil)))
        (recur (rest col) (inc id) true (conj acc (repeat (first col) id)))))))

(defn part-2 [input]
  (->> (expand-disk-2 input)
       do-swaps-by-length
       flatten
       (map-indexed (fn [idx n] (if (nil? n) 0 (* idx n))))
       (reduce +))) ;;6399865217220 too high