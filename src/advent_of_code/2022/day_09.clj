(ns advent-of-code.2022.day-09
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2022/day_09.txt")
               (map #(str/split % #" "))
               (map (fn [[dir amt]] [dir (Integer/parseInt amt)]))))

(defn go-left [[x y]] [(dec x) y])
(defn go-right [[x y]] [(inc x) y])
(defn go-up [[x y]] [x (inc y)])
(defn go-down [[x y]] [x (dec y)])

(defn next-point [head tail]
  (let [[hx hy] head
        [tx ty] tail
        xDiff (- hx tx)
        yDiff (- hy ty)]
    (if (or (> (Math/abs xDiff) 1) (> (Math/abs yDiff) 1))
      [(+ tx (cond
               (> xDiff 0) 1
               (= xDiff 0) 0
               (< xDiff 0) -1))
       (+ ty (cond
               (> yDiff 0) 1
               (= yDiff 0) 0
               (< yDiff 0) -1))]
      tail)))

(defn move-tail [rope]
  (cond
    (empty? rope) []
    (= 1 (count rope)) rope
    :else (let [next-tail (next-point (first rope) (second rope))]
            (concat [(first rope)] (move-tail (concat [next-tail] (rest (rest rope))))))))

(defn do-moves [input-ropes]
  (loop [moves data
         ropes input-ropes
         visited []]
    (if (empty? moves)
      (count (set visited))
      (let [[dir amt] (first moves)]
        (cond
         (= amt 0)
         (recur (rest moves) ropes visited)

         (= dir "R")
         (let [new-ropes (move-tail (concat [(go-right (first ropes))] (rest ropes)))]
           (recur (conj (rest moves) [dir (dec amt)])
                  new-ropes
                  (conj visited (last new-ropes))))

         (= dir "L")
         (let [new-ropes (move-tail (concat [(go-left (first ropes))] (rest ropes)))]
           (recur (conj (rest moves) [dir (dec amt)])
                  new-ropes
                  (conj visited (last new-ropes))))

         (= dir "U")
         (let [new-ropes (move-tail (concat [(go-up (first ropes))] (rest ropes)))]
           (recur (conj (rest moves) [dir (dec amt)])
                  new-ropes
                  (conj visited (last new-ropes))))

         (= dir "D")
         (let [new-ropes (move-tail (concat [(go-down (first ropes))] (rest ropes)))]
           (recur (conj (rest moves) [dir (dec amt)])
                  new-ropes
                  (conj visited (last new-ropes)))))))))

(defn part-1 [] (do-moves [[0 0] [0 0]])) ;; 5902
(defn part-2 [] (do-moves [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]])) ;; 2445
