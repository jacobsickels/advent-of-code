(ns advent-of-code.2018.day-08
  (:require [advent-of-code.shared.read-file :as read]))

(def test-data [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn go-level [col input-meta pointer]
  (loop [m input-meta
         p pointer]
    (let [num-children (first (drop pointer col))
          num-meta (second (drop pointer col))
          top (drop (+ pointer 2) col)]
      (println "TOP" top "num-children" num-children)
      (cond

        (empty? top) m

        (zero? num-children)
        (recur (+ m (reduce + (take num-meta top)))
               (+ p num-meta))

        ;(zero? count-children)
        ;[(+ m (reduce + data)) (count data)]

        :else (let [new-meta (go-level top m (+ p 2))]
                (println "new-meta" new-meta)
                (recur new-meta p))))))

;; Maybe store meta-to-get and num-children on stacks that pop when you move back up a level
;; Go from left to right without recursion, could store :depth


