(ns advent-of-code.2018.day-16
  (:require [advent-of-code.shared.read-file :as read]))

(defn- addr [[op-code a b c] registers]
  [op-code (assoc registers c (+ (get registers a)
                                 (get registers b)))])

(defn- addi [[op-code a b c] registers]
  [op-code (assoc registers c (+ (get registers a) b))])

(defn- mulr [[op-code a b c] registers]
  [op-code (assoc registers c (* (get registers a)
                                 (get registers b)))])

(defn- muli [[op-code a b c] registers]
  [op-code (assoc registers c (* (get registers a) b))])

(defn- banr [[op-code a b c] registers]
  [op-code (assoc registers c (bit-and (get registers a)
                                       (get registers b)))])

(defn- bani [[op-code a b c] registers]
  [op-code (assoc registers c (bit-and (get registers a) b))])

(defn- borr [[op-code a b c] registers]
  [op-code (assoc registers c (bit-or (get registers a)
                                      (get registers b)))])

(defn- bori [[op-code a b c] registers]
  [op-code (assoc registers c (bit-or (get registers a) b))])

(defn- setr [[op-code a b c] registers]
  [op-code (assoc registers c (get registers a))])

(defn- seti [[op-code a b c] registers]
  [op-code (assoc registers c a)])

(defn- gtir [[op-code a b c] registers]
  [op-code (if (> a (get registers b))
             (assoc registers c 1)
             (assoc registers c 0))])

(defn- gtri [[op-code a b c] registers]
  [op-code (if (> (get registers a) b)
             (assoc registers c 1)
             (assoc registers c 0))])

(defn- gtrr [[op-code a b c] registers]
  [op-code (if (> (get registers a) (get registers b))
             (assoc registers c 1)
             (assoc registers c 0))])

(defn- eqir [[op-code a b c] registers]
  [op-code (if (= a (get registers b))
             (assoc registers c 1)
             (assoc registers c 0))])

(defn- eqri [[op-code a b c] registers]
  [op-code (if (= (get registers a) b)
             (assoc registers c 1)
             (assoc registers c 0))])

(defn- eqrr [[op-code a b c] registers]
  [op-code (if (= (get registers a) (get registers b))
             (assoc registers c 1)
             (assoc registers c 0))])

(defn- eqrr [[op-code a b c] registers]
  [op-code (if (= (get registers a) (get registers b))
             (assoc registers c 1)
             (assoc registers c 0))])

(defn parse-line [line]
  (->> (map #(re-seq #"-?\d+" %) line)
       (mapv #(mapv (fn [i] (Integer/parseInt i)) %))))

(def part-1-data (->> (read/read-file "resources/2018/day_16_1.txt")
                      (partition-by empty?)
                      (remove #(= 1 (count %)))
                      (map parse-line)))

(defn acts-as-function? [func [before instruction after]]
  (= (second (func instruction before)) after))

(defn get-acting-functions [[before instruction after] remove-instructions]
  (apply dissoc {"addr" (acts-as-function? addr [before instruction after])
                 "addi" (acts-as-function? addi [before instruction after])
                 "mulr" (acts-as-function? mulr [before instruction after])
                 "muli" (acts-as-function? muli [before instruction after])
                 "banr" (acts-as-function? banr [before instruction after])
                 "bani" (acts-as-function? bani [before instruction after])
                 "borr" (acts-as-function? borr [before instruction after])
                 "bori" (acts-as-function? bori [before instruction after])
                 "setr" (acts-as-function? setr [before instruction after])
                 "seti" (acts-as-function? seti [before instruction after])
                 "gtir" (acts-as-function? gtir [before instruction after])
                 "gtri" (acts-as-function? gtri [before instruction after])
                 "gtrr" (acts-as-function? gtrr [before instruction after])
                 "eqir" (acts-as-function? eqir [before instruction after])
                 "eqri" (acts-as-function? eqri [before instruction after])
                 "eqrr" (acts-as-function? eqrr [before instruction after])}
         remove-instructions))

(defn acts-like-3-or-more? [[before instruction after]]
   (-> (get-acting-functions [before instruction after] [])
       vals
       frequencies
       (get true)
       (or 0)
       (>= 3)))

(defn part-1 []
  (count (filter acts-like-3-or-more? part-1-data)))

(defn is-acting-on [[before instruction after]]
  (println [before instruction after])
  [(first instruction) (->> (get-acting-functions [before instruction after] [])
                            (filter #(true? (second %)))
                            (map first))])

;; Found using is-acting-on and dissocing ones that were found from repl
(def mapped-instructions {15 addi
                          14 banr
                          13 muli
                          12 bani
                          11 mulr
                          10 addr
                          9 borr
                          8 eqrr
                          7 eqri
                          6 seti
                          5 eqir
                          4 gtri
                          3 gtrr
                          1 setr
                          2 bori
                          0 gtir})

(def part-2-data (->> (read/read-file "resources/2018/day_16_2.txt")
                      (map #(re-seq #"-?\d+" %))
                      (map #(map (fn [x] (Integer/parseInt x)) %))))

(defn part-2 []
  (loop [registers [0 0 0 0]
         instructions part-2-data]
    (if (empty? instructions)
      registers
      (let [instruction (first instructions)
            func (get mapped-instructions (first instruction))]
        (recur (second (func instruction registers))
               (rest instructions))))))

