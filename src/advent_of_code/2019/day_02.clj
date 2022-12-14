(ns advent-of-code.2019.day-02)

(defn get-math [num]
  (cond
    (= num 1) +
    (= num 2) *))

(defn program-alarm [input]
  (loop [program (aclone input)
         pos 0]
    (if (= (get program pos) 99)
      program
      (do
        (aset program
              (get program (+ pos 3))
              ((get-math (get program pos)) (get program (get program (+ pos 1))) (get program (get program (+ pos 2)))))
        (recur program (+ pos 4))))))


(defn program-alarm-args
  [input noun verb]
  (let [program (aclone input)]
    (do
      (aset program 1 noun)
      (aset program 2 verb)
      (get (program-alarm program) 0))))

(defn find-noun-verb
  [input]
  (loop [noun 0
         verb 0]
    (if (= (program-alarm-args input noun verb) 19690720)
      (println noun verb)
      (if (>= verb 100)
        (recur (inc noun) 0)
        (recur noun (inc verb))))))