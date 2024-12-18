(ns advent-of-code.2024.day-17)

(defn get-combo-value [registers combo]
  (cond
    (contains? #{0 1 2 3} combo) combo
    (= 4 combo) (:a registers)
    (= 5 combo) (:b registers)
    (= 6 combo) (:c registers)

    :else nil))

(defn big-xor
  [f & r]
  (reduce (fn [acc v] (.xor acc (biginteger v))) (biginteger f) r))

(defn adv [registers combo pointer]
  [(assoc registers :a (quot (:a registers) (Math/pow 2 (get-combo-value registers combo)))) (+ 2 pointer)])

(defn bxl [registers combo pointer]
  [(assoc registers :b (big-xor (:b registers) combo)) (+ 2 pointer)])

(defn bst [registers combo pointer]
  [(assoc registers :b (mod (get-combo-value registers combo) 8)) (+ 2 pointer)])

(defn jnz [registers combo pointer]
  (if (zero? (:a registers))
    [registers pointer true]
    [registers combo]))

(defn bxc [registers combo pointer]
  [(assoc registers :b (big-xor (:b registers) (:c registers))) (+ 2 pointer)])

(defn out [registers combo pointer]
  [(assoc registers :out (conj (:out registers) (mod (get-combo-value registers combo) 8))) (+ 2 pointer)])

(defn bdv [registers combo pointer]
  [(assoc registers :b (quot (:a registers) (Math/pow 2 (get-combo-value registers combo)))) (+ 2 pointer)])

(defn cdv [registers combo pointer]
  [(assoc registers :c (quot (:a registers) (Math/pow 2 (get-combo-value registers combo)))) (+ 2 pointer)])

(defn do-instruction [registers opcode combo pointer]
  (cond
    (= opcode 0) (adv registers combo pointer)
    (= opcode 1) (bxl registers combo pointer)
    (= opcode 2) (bst registers combo pointer)
    (= opcode 3) (jnz registers combo pointer)
    (= opcode 4) (bxc registers combo pointer)
    (= opcode 5) (out registers combo pointer)
    (= opcode 6) (bdv registers combo pointer)
    (= opcode 7) (cdv registers combo pointer)
    :else nil))


(def test-data [0 1 5 4 3 0])
(def test-registers {:a   729
                     :b   0
                     :c   0
                     :out []})

(def test-data-2 [0 3 5 4 3 0])
(def test-registers-2 {:a   117440
                       :b   0
                       :c   0
                       :out []})



(def data [2 4 1 1 7 5 1 5 4 5 0 3 5 5 3 0])
(def data-registers {:a   30344604
                     :b   0
                     :c   0
                     :out []})
;; [4 3 2 6 4 5 3 2 4]

;; [1 3 2 6 4 5 3 2 4] +2
;; [1 3 2 6 4 5 3 2 4] -1

;; [0 3 2 6 4 5 3 2 4] -4
;; [0 3 2 6 4 5 3 2 4] +1

;; [5 3 2 6 4 5 3 2 4] -2
;; [5 3 2 6 4 5 3 2 4] +3

;; [4 3 2 6 4 5 3 2 4] -3
;; [4 5 2 6 4 5 3 2 4] +4

(defn run-program [input input-registers]
  (let [instructions input]
    (loop [registers input-registers
           pointer 0
           done nil]
      (println registers)
      (if done
        (:out registers)
        (if (= pointer (dec (count instructions)))
          registers
          (let [[new-registers new-pointer done?] (do-instruction registers
                                                                  (nth instructions pointer)
                                                                  (nth instructions (inc pointer))
                                                                  pointer)]
            (recur new-registers new-pointer done?)))))))


(defn part-1 [input input-registers]
  (run-program input input-registers))

(defn part-2 [a-register]
  (let [registers {:a a-register :b 0 :c 0 :out []}]
     (run-program data registers)))
