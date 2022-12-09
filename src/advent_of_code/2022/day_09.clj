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

(defn move-tail [head tail]
  (let [[hx hy] (last head)
        [tx ty] (last tail)
        xDiff (Math/abs (- hx tx))
        yDiff (Math/abs (- hy ty))]
    (if (or (> xDiff 1) (> yDiff 1))
      (conj tail (last (butlast head)))
      tail)))

(defn do-moves []
  (loop [moves data
         head [[0 0]]
         tail [[0 0]]]
    (if (empty? moves)
      [head tail]
      (let [[dir amt] (first moves)]
        (cond
          (= amt 0)
          (recur (rest moves) head tail)

          (= dir "R")
          (let [new-head (conj head (go-right (last head)))]
            (recur (conj (rest moves) [dir (dec amt)])
                   new-head
                   (move-tail new-head tail)))

          (= dir "L")
          (let [new-head (conj head (go-left (last head)))]
            (recur (conj (rest moves) [dir (dec amt)])
                   new-head
                   (move-tail new-head tail)))

          (= dir "U")
          (let [new-head (conj head (go-up (last head)))]
            (recur (conj (rest moves) [dir (dec amt)])
                   new-head
                   (move-tail new-head tail)))

          (= dir "D")
          (let [new-head (conj head (go-down (last head)))]
            (recur (conj (rest moves) [dir (dec amt)])
                   new-head
                   (move-tail new-head tail))))))))

(defn part-1 []
  (let [[head tail] (do-moves)]
    (count (set tail))))

(defn move-tail-2 [head tail]
  (let [[hx hy] (last head)
        [tx ty] (last tail)
        xDiff (- hx tx)
        yDiff (- hy ty)]

    (if (or (> (Math/abs xDiff) 1) (> (Math/abs yDiff) 1))
      (conj tail [(+ tx (cond
                          (> xDiff 0) 1
                          (= xDiff 0) 0
                          (< xDiff 0) -1))
                  (+ ty (cond
                          (> yDiff 0) 1
                          (= yDiff 0) 0
                          (< yDiff 0) -1))])
      tail)))

(defn move-direction [ropes direction-fn]
  (let [head (nth ropes 0)
        one (nth ropes 1)
        two (nth ropes 2)
        three (nth ropes 3)
        four (nth ropes 4)
        five (nth ropes 5)
        six (nth ropes 6)
        seven (nth ropes 7)
        eight (nth ropes 8)
        nine (nth ropes 9)
        new-head (conj head (direction-fn (last head)))
        new-one (move-tail-2 new-head one)
        new-two (move-tail-2 new-one two)
        new-three (move-tail-2 new-two three)
        new-four (move-tail-2 new-three four)
        new-five (move-tail-2 new-four five)
        new-six (move-tail-2 new-five six)
        new-seven (move-tail-2 new-six seven)
        new-eight (move-tail-2 new-seven eight)
        new-nine (move-tail-2 new-eight nine)]
    [new-head new-one new-two new-three new-four new-five new-six new-seven new-eight new-nine]))

(defn do-moves-2 []
  (loop [moves data
         ropes [[[0 0]]
                [[0 0]]
                [[0 0]]
                [[0 0]]
                [[0 0]]
                [[0 0]]
                [[0 0]]
                [[0 0]]
                [[0 0]]
                [[0 0]]]]
    (if (empty? moves)
      ropes
      (let [[dir amt] (first moves)]
        (cond
          (= amt 0)
          (recur (rest moves) ropes)

          (= dir "R")
          (recur (conj (rest moves) [dir (dec amt)])
                 (move-direction ropes go-right))

          (= dir "L")
          (recur (conj (rest moves) [dir (dec amt)])
                 (move-direction ropes go-left))

          (= dir "U")
          (recur (conj (rest moves) [dir (dec amt)])
                 (move-direction ropes go-up))

          (= dir "D")
          (recur (conj (rest moves) [dir (dec amt)])
                 (move-direction ropes go-down)))))))

(defn part-2 [] (count (set (last (do-moves-2)))))