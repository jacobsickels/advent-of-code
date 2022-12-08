(ns advent-of-code.2022.day-07
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [clojure.set :as set]))

(def data (->> (read/read-file "resources/2022/day_07.txt")
               (map #(str/split % #" "))))

(defn cd-command [commands dir-stack directory]
  (let [[_ _ c] (first commands)]
    (cond
      (= c "/")
      [(rest commands) ["/"] directory]

      (= c "..")
      [(rest commands) (vec (butlast dir-stack)) directory]

      :else
      [(rest commands) (conj dir-stack c) directory])))

(defn dir-command [commands dir-stack directory]
  (let [[_ b _] (first commands)]
    [(rest commands) dir-stack (assoc-in directory (conj dir-stack b) {})]))

(defn file-command [commands dir-stack directory files]
  (let [[a b _] (first commands)]
    [(rest commands)
     dir-stack
     (assoc-in directory (conj dir-stack b) (Integer/parseInt a))
     (conj files [dir-stack (Integer/parseInt a)])]))


(defn is-number? [s] (every? #(Character/isDigit %) s))

(defn get-directory-structure []
  (loop [commands data
         dir-stack []
         directory {}
         files []]
    (if (empty? commands)
      files
      (let [[a b c] (first commands)]
        (cond
          (= b "cd")
          (let [[new-commands new-dir-stack new-directory] (cd-command commands dir-stack directory)]
            (recur new-commands new-dir-stack new-directory files))

          (= a "dir")
          (let [[new-commands new-dir-stack new-directory] (dir-command commands dir-stack directory)]
            (recur new-commands new-dir-stack new-directory files))

          (is-number? a)
          (let [[new-commands new-dir-stack new-directory new-files] (file-command commands dir-stack directory files)]
            (recur new-commands new-dir-stack new-directory new-files))

          :else
          (recur (rest commands)
                 dir-stack
                 directory
                 files))))))

(defn get-sub-directories [directory]
  (loop [dirs (reverse directory)
         acc []]
    (if (empty? dirs)
      acc
      (recur (rest dirs)
             (conj acc (reverse dirs))))))

(defn get-file-directories [files]
  (mapcat get-sub-directories files))

(defn directory-sizes []
  (let [files (get-directory-structure)
        directories (->> (set (get-file-directories (map first files)))
                         (map #(apply str %)))
        files (map #(list (apply str (first %)) (second %)) files)]
    (->> directories
         (map (fn [dir] (filter #(str/starts-with? (first %) dir) files)))
         (map #(list (ffirst %) (reduce (fn [acc [dir numb]] (+ acc numb)) 0 %))))))

(defn part-1 []
  (->> (directory-sizes)
       (filter #(<= (second %) 100000))
       (map second)
       (reduce +)))

(defn part-2 []
  (let [dir-sizes (directory-sizes)
        root-size (second (first (filter #(= (first %) "/") dir-sizes)))
        unused-space (- 70000000 root-size)
        need-to-free (- 30000000 unused-space)]
    (->> (filter #(>= (second %) need-to-free) dir-sizes)
         (sort-by second)
         first
         second)))

(defn col-starts-with? [col subcol]
  (and (every? true? (map #(= %1 %2) col subcol))
       (<= (count subcol) (count col))))



