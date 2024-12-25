(ns advent-of-code.2024.day-24
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_24.txt")
               (partition-by empty?)
               (remove #(= "" (first %)))))

(def wires (->> (first data)
                (reduce (fn [acc s]
                          (let [[n v] (str/split s #": ")]
                            (assoc acc n (Integer/parseInt v))))
                        {})))

(def gates (->> (second data)
                (map #(let [[gate output] (str/split % #" -> ")
                            [a type b] (str/split gate #" ")]
                        { :output output :a a :b b :type type}))
                (sort-by :output)))


(defn AND [a b] (if (= 1 a b) 1 0))
(defn OR [a b] (if (or (= 1 a) (= 1 b)) 1 0))
(defn XOR [a b] (if (= #{1 0} (conj #{} a b)) 1 0))


(defn do-gate [gate output-wires]
  (let [a (get output-wires (:a gate))
        b (get output-wires (:b gate))
        type (:type gate)
        output (:output gate)]
    (if (or (nil? a) (nil? b))
      output-wires ;; can't do this gate yet
      (cond
        (= type "AND")
        (assoc output-wires output (AND a b))

        (= type "OR")
        (assoc output-wires output (OR a b))

        (= type "XOR")
        (assoc output-wires output (XOR a b))))))

(defn part-1 [wires gates]
  (loop [g gates
         w (merge wires (reduce (fn [acc g] (assoc acc (:output g) nil)) {} gates))]
    (let [z-outputs (->> (filter (fn [[k v]] (str/starts-with? k "z")) w))]
      (if (not (contains? (set (map last z-outputs)) nil))
        (Long/parseLong (->> (sort-by first z-outputs)
                             reverse
                             (map last)
                             (apply str)) 2)
        (if (empty? g)
          (recur gates w)
          (let [new-wires (do-gate (first g) w)]
            (recur (rest g) new-wires)))))))

(defn get-wire [x-val y-val]
  (let [[x-keys y-keys] (->> (keys wires)
                             sort
                             (partition-by first))
        x-string (Long/toBinaryString x-val)
        y-string (Long/toBinaryString y-val)]
    (merge (into {} (map-indexed (fn [idx k] (vector k (Integer/parseInt (str (nth x-string idx 0))))) x-keys))
           (into {} (map-indexed (fn [idx k] (vector k (Integer/parseInt (str (nth y-string idx 0))))) y-keys)))))


(defn return-outputs [wires gates]
  (loop [g gates
         w (merge wires (reduce (fn [acc g] (assoc acc (:output g) nil)) {} gates))]
    (let [z-outputs (->> (filter (fn [[k v]] (str/starts-with? k "z")) w))]
      (if (not (contains? (set (map last z-outputs)) nil))
        (->> (sort-by first z-outputs)
             reverse
             (map last)
             (apply str))
        (if (empty? g)
          (recur gates w)
          (let [new-wires (do-gate (first g) w)]
            (recur (rest g) new-wires)))))))

;(defn part-2 []
;  (let [x (Long/parseLong (->> (filter (fn [[k v]] (str/starts-with? k "x")) wires)
;                               (sort-by first)
;                               reverse
;                               (map last)
;                               (apply str)) 2)
;        y (Long/parseLong (->> (filter (fn [[k v]] (str/starts-with? k "y")) wires)
;                               (sort-by first)
;                               reverse
;                               (map last)
;                               (apply str)) 2)]
;    (- (part-1 gates) (+ x y)
;       (Long/parseLong (str "1" (apply str (repeat 33 "0"))) 2)
;       (Long/parseLong (str "1" (apply str (repeat 25 "0"))) 2)
;       (Long/parseLong (str "1" (apply str (repeat 21 "0"))) 2))))

(defn swap-outputs [gates output-1 output-2]
  (let [gate-1 (first (filter #(= (:output %) output-1) gates))
        gate-2 (first (filter #(= (:output %) output-2) gates))]
    (conj (remove #(or (= (:output %) output-1) (= (:output %) output-2)) gates)
          (assoc gate-1 :output output-2)
          (assoc gate-2 :output output-1))))


(defn get-gate-from-output [output]
  (first (filter #(= (:output %) output) gates)))

(defn make-map [gate]
  (let [inputs [(:a gate) (:b gate)]
        next-gates (->> (map get-gate-from-output inputs)
                        (remove nil?))]
    (if (empty? next-gates)
      []
      (reduce (fn [acc g]
                (let [next-map (make-map g)]
                  (assoc-in acc [(:output gate) (:output g)] next-map)))
              {(:output gate) {}}
              next-gates))))

(defn make-mermaid-lines []
  (str/join (mapcat (fn [g] [(str (:a g) "->" (:output g) ";") (str (:b g) "->" (:output g) ";")])
                    (-> gates
                        (swap-outputs "nhn" "z21")
                        (swap-outputs "gst" "z33")
                        #_(swap-outputs "khg" "z25")))))


(defn part-2 []
  (let [
        pow 36
        wires wires
        ;wires (get-wire (dec (Math/pow 2 pow)) (dec (Math/pow 2 pow)))
        x (Long/parseLong (->> (filter (fn [[k v]] (str/starts-with? k "x")) wires)
                               (sort-by first)
                               reverse
                               (map last)
                               (apply str)) 2)
        y (Long/parseLong (->> (filter (fn [[k v]] (str/starts-with? k "y")) wires)
                               (sort-by first)
                               reverse
                               (map last)
                               (apply str)) 2)
        g (sort-by :output (-> gates
                               (swap-outputs "vdc" "z12")
                               (swap-outputs "nhn" "z21")
                               (swap-outputs "gst" "z33")
                               (swap-outputs "khg" "tvb")))
        plus (Long/toBinaryString (+ x y))
        evaluated (Long/toBinaryString (part-1 wires g))]

    [evaluated plus (= plus evaluated)]))

;; ANSWER  "gst,khg,nhn,tvb,vdc,z12,z21,z33"


(defn get-char-outputs [col c]
  (->> (filter (fn [[k v]] (str/starts-with? k c)) col)
       (remove #(nil? (second %)))
       (sort-by first)))

(defn is-adder-correct? [col]
  (let [z-outputs (get-char-outputs col "z")
        x-outputs (get-char-outputs col "x")
        y-outputs (get-char-outputs col "y")]
    (println z-outputs (take (count z-outputs) x-outputs) (take (count z-outputs) y-outputs))
    true))


(defn return-outputs [gates]
  (loop [g gates
         w (merge wires (reduce (fn [acc g] (assoc acc (:output g) nil)) {} gates))]
    (if (not (is-adder-correct? w))
      w
      (let [z-outputs (->> (filter (fn [[k v]] (str/starts-with? k "z")) w))]
        (if (not (contains? (set (map last z-outputs)) nil))
          (->> (sort-by first z-outputs)
               reverse
               (map last)
               (apply str))
          (if (empty? g)
            (recur gates w)
            (let [new-wires (do-gate (first g) w)]
              (recur (rest g) new-wires))))))))






;["kcp" "z12"]
;["nhn" "z21"]

;; https://dreampuf.github.io/GraphvizOnline/?engine=dot#%0Adigraph%20G%20%7B%0A%20%20subgraph%20input_x%20%7B%0A%20%20%20%20node%20%5Bstyle%3Dfilled%2Ccolor%3Dlightgrey%5D%3B%0A%20%20%20%20x00%20-%3E%20x01%20-%3E%20x02%20-%3E%20x03%20-%3E%20x04%20-%3E%20x05%20-%3E%20x06%20-%3E%20x07%20-%3E%20x08%20-%3E%20x09%20-%3E%20x10%20-%3E%20x11%20-%3E%20x12%20-%3E%20x13%20-%3E%20x14%20-%3E%20x15%20-%3E%20x16%20-%3E%20x17%20-%3E%20x18%20-%3E%20x19%20-%3E%20x20%20-%3E%20x21%20-%3E%20x22%20-%3E%20x23%20-%3E%20x24%20-%3E%20x25%20-%3E%20x26%20-%3E%20x27%20-%3E%20x28%20-%3E%20x29%20-%3E%20x30%20-%3E%20x31%20-%3E%20x32%20-%3E%20x33%20-%3E%20x34%20-%3E%20x35%20-%3E%20x36%20-%3E%20x37%20-%3E%20x38%20-%3E%20x39%20-%3E%20x40%20-%3E%20x41%20-%3E%20x42%20-%3E%20x43%20-%3E%20x44%20-%3E%20x45%3B%0A%20%20%7D%0A%20%20subgraph%20input_y%20%7B%0A%20%20%20%20node%20%5Bstyle%3Dfilled%2Ccolor%3Dlightgrey%5D%3B%0A%20%20%20%20y00%20-%3E%20y01%20-%3E%20y02%20-%3E%20y03%20-%3E%20y04%20-%3E%20y05%20-%3E%20y06%20-%3E%20y07%20-%3E%20y08%20-%3E%20y09%20-%3E%20y10%20-%3E%20y11%20-%3E%20y12%20-%3E%20y13%20-%3E%20y14%20-%3E%20y15%20-%3E%20y16%20-%3E%20y17%20-%3E%20y18%20-%3E%20y19%20-%3E%20y20%20-%3E%20y21%20-%3E%20y22%20-%3E%20y23%20-%3E%20y24%20-%3E%20y25%20-%3E%20y26%20-%3E%20y27%20-%3E%20y28%20-%3E%20y29%20-%3E%20y30%20-%3E%20y31%20-%3E%20y32%20-%3E%20y33%20-%3E%20y34%20-%3E%20y35%20-%3E%20y36%20-%3E%20y37%20-%3E%20y38%20-%3E%20y39%20-%3E%20y40%20-%3E%20y41%20-%3E%20y42%20-%3E%20y43%20-%3E%20y44%20-%3E%20y45%3B%0A%20%20%7D%0A%20%20subgraph%20gates_and%20%7B%0A%20%20%20%20node%20%5Bstyle%3Dfilled%2Ccolor%3Dlightgreen%5D%3B%0A%20%20%20%20x23%3By23%3Bx41%3By41%3Bkbv%3Bctw%3Bnmw%3Bbgp%3By29%3Bx29%3By17%3Bx17%3Bkmm%3Bptk%3Bhbr%3Bqfp%3Bqfk%3Bpvj%3Brqd%3Bcns%3Bjbv%3Bkjg%3Bvwd%3Bkvv%3Bwft%3Bnnq%3Bsrk%3Bsjm%3By06%3Bx06%3Bfgc%3Bdvm%3By03%3Bx03%3By02%3Bx02%3By26%3Bx26%3Brrj%3Bmft%3Bx16%3By16%3Bnqm%3Bfmf%3By13%3Bx13%3By28%3Bx28%3Bcsf%3Bbck%3Bhhg%3Bccp%3Bx42%3By42%3Bqtg%3Bknh%3Bx27%3By27%3Bfpk%3Bdfh%3Bx38%3By38%3Bvbq%3Bbwv%3Bfjn%3Bhtc%3Bmmw%3Bncf%3By11%3Bx11%3By08%3Bx08%3Bfbk%3Btcq%3Bx20%3By20%3Bx36%3By36%3Bx12%3By12%3By32%3Bx32%3By04%3Bx04%3Brtg%3Bkfd%3Bcbk%3Bnbm%3Bx09%3By09%3Bjbr%3Bwcs%3By00%3Bx00%3By39%3Bx39%3Bbbn%3Brsc%3Btvm%3Bpkw%3Bx19%3By19%3By05%3Bx05%3Bx31%3By31%3Bx15%3By15%3Bjmr%3Bnhm%3Bx10%3By10%3Bbgq%3Bwdh%3Bx44%3By44%3By43%3Bx43%3By30%3Bx30%3Btvb%3Bjhd%3Bx40%3By40%3Bfdv%3Bdfs%3Bpmv%3Bgqg%3By24%3Bx24%3Bfcf%3Bspk%3Bfqc%3Bcpt%3By22%3Bx22%3Bmfp%3Bspf%3Bgpk%3Bspq%3Bx21%3By21%3Bspg%3Bhhm%3Bmcv%3Bmbp%3Bwdg%3Bpps%3Bx33%3By33%3By25%3Bx25%3By34%3Bx34%3Bfvh%3Bpgc%3Bjkq%3Bnhn%3Bdbf%3Bdsb%3By14%3Bx14%3Bx35%3By35%3By37%3Bx37%3By07%3Bx07%3By01%3Bx01%3Bchs%3Brgv%3By18%3Bx18%3Bddw%3Bfht%3Bgsj%3Bftw%3B%0A%20%20%7D%0A%20%20subgraph%20gates_or%20%7B%0A%20%20%20%20node%20%5Bstyle%3Dfilled%2Ccolor%3Dyellow%5D%3B%0A%20%20%20%20djs%3Btgk%3Bjns%3Bnpn%3Bcdc%3Bstq%3Bccd%3Bcst%3Bndp%3Bnfq%3Bbsm%3Bvsc%3Bptj%3Bdtg%3Bstm%3Btjt%3Brcb%3Bkhg%3Bggg%3Bhvg%3Bjdk%3Bpmr%3Bbvk%3Bkhh%3Bwgd%3Bvjg%3Bqss%3Bwtt%3Bdmb%3Bcrv%3Bnhh%3Btgq%3Btnt%3Bkhn%3Bpkj%3Bmsp%3Btvc%3Bdcc%3Bvnr%3Bkrb%3Bpvc%3Bgst%3Bdbm%3Bjft%3Btbn%3Bgrb%3Bqhg%3Bgnc%3Bmgw%3Bbds%3Brkf%3Bmhq%3Bmjr%3Bkpb%3Bjbq%3Bvmj%3Bqfn%3Bfct%3Bcnm%3Bdgh%3Bvdc%3Bkcp%3Bssj%3Bccr%3Bfwg%3Bgtf%3Bjhv%3Bnph%3Bgqb%3Bcsh%3Bcbf%3Bnfn%3Brjq%3Bnfh%3Bnrq%3Bvkg%3Bpwp%3Bnmj%3Bhdq%3Bgdm%3Bdwt%3Bqsg%3Bsdp%3Bptd%3Bbcr%3Bpsw%3Brvg%3Bwvv%3B%0A%20%20%7D%0A%20%20subgraph%20gates_xor%20%7B%0A%20%20%20%20node%20%5Bstyle%3Dfilled%2Ccolor%3Dlightskyblue%5D%3B%0A%20%20%20%20dbf%3Bdsb%3Btvb%3Bjhd%3By29%3Bx29%3Btvm%3Bpkw%3Bnmw%3Bbgp%3Bx16%3By16%3Bx37%3By37%3Bx24%3By24%3By40%3Bx40%3Bfht%3Bddw%3Bgqg%3Bpmv%3Bx15%3By15%3Bbwv%3Bvbq%3Bkbv%3Bctw%3Bx10%3By10%3Bx08%3By08%3Bx26%3By26%3Bx36%3By36%3Bmmw%3Bncf%3Bx05%3By05%3Bwft%3Bnnq%3Bx31%3By31%3Brrj%3Bmft%3By00%3Bx00%3Bx25%3By25%3Bpps%3Bwdg%3By04%3Bx04%3Bnhm%3Bjmr%3Bx06%3By06%3Bx22%3By22%3Bx14%3By14%3Bkfd%3Brtg%3Bx34%3By34%3Bcpt%3Bfqc%3Bspq%3Bgpk%3Bx02%3By02%3Bmfp%3Bspf%3Bspg%3Bhhm%3Bfmf%3Bnqm%3Bfvh%3Bpgc%3Bfcf%3Bspk%3Bx07%3By07%3Bptk%3Bkmm%3Bx28%3By28%3By42%3Bx42%3By19%3Bx19%3Bkvv%3Bvwd%3Bwcs%3Bjbr%3Bsrk%3Bsjm%3Bbgq%3Bwdh%3By18%3Bx18%3Bx23%3By23%3Bjbv%3Bkjg%3Bx32%3By32%3Bccp%3Bhhg%3Bftw%3Bgsj%3Bfjn%3Bhtc%3Bpvj%3Bqfk%3Bx21%3By21%3Bx38%3By38%3Bjkq%3Bnhn%3Bx30%3By30%3By35%3Bx35%3Bx27%3By27%3Bchs%3Brgv%3By39%3Bx39%3Bx13%3By13%3Bknh%3Bqtg%3By17%3Bx17%3Bhbr%3Bqfp%3By01%3Bx01%3Bdfs%3Bfdv%3Bdvm%3Bfgc%3By03%3Bx03%3Bx44%3By44%3Bcns%3Brqd%3Bx20%3By20%3By43%3Bx43%3Bdfh%3Bfpk%3Bfbk%3Btcq%3Bx09%3By09%3Bx11%3By11%3Bmcv%3Bmbp%3Bnbm%3Bcbk%3Brsc%3Bbbn%3Bx41%3By41%3Bbck%3Bcsf%3Bx33%3By33%3Bx12%3By12%3B%0A%20%20%7D%0A%20%20%0A%20%20subgraph%20output_z%20%7B%0A%20%20%20%20%20%20node%20%5Bstyle%3Dfilled%2Ccolor%3Dlightgreen%5D%3B%0A%20%20%20%20z00%20-%3E%20z01%20-%3E%20z02%20-%3E%20z03%20-%3E%20z04%20-%3E%20z05%20-%3E%20z06%20-%3E%20z07%20-%3E%20z08%20-%3E%20z09%20-%3E%20z10%20-%3E%20z11%20-%3E%20z12%20-%3E%20z13%20-%3E%20z14%20-%3E%20z15%20-%3E%20z16%20-%3E%20z17%20-%3E%20z18%20-%3E%20z19%20-%3E%20z20%20-%3E%20z21%20-%3E%20z22%20-%3E%20z23%20-%3E%20z24%20-%3E%20z25%20-%3E%20z26%20-%3E%20z27%20-%3E%20z28%20-%3E%20z29%20-%3E%20z30%20-%3E%20z31%20-%3E%20z32%20-%3E%20z33%20-%3E%20z34%20-%3E%20z35%20-%3E%20z36%20-%3E%20z37%20-%3E%20z38%20-%3E%20z39%20-%3E%20z40%20-%3E%20z41%20-%3E%20z42%20-%3E%20z43%20-%3E%20z44%20-%3E%20z45%3B%0A%20%20%7D%0A%20%20%0A%20%20%0Arsc-%3Ez21%3Bbbn-%3Ez21%3Bcdc-%3Enhn%3Bstq-%3Enhn%3Bx12-%3Ekcp%3By12-%3Ekcp%3Bwdg-%3Ez12%3Bpps-%3Ez12%3Bx23-%3Emhq%3By23-%3Emhq%3Bdbf-%3Ez35%3Bdsb-%3Ez35%3Bx41-%3Eqss%3By41-%3Eqss%3Bkbv-%3Eqhg%3Bctw-%3Eqhg%3Bdjs-%3Eccp%3Btgk-%3Eccp%3Bnmw-%3Ebcr%3Bbgp-%3Ebcr%3Bjns-%3Ejbv%3Bnpn-%3Ejbv%3By29-%3Etjt%3Bx29-%3Etjt%3By17-%3Enhh%3Bx17-%3Enhh%3Bkmm-%3Evnr%3Bptk-%3Evnr%3Bhbr-%3Emjr%3Bqfp-%3Emjr%3Bqfk-%3Endp%3Bpvj-%3Endp%3Btvb-%3Ez25%3Bjhd-%3Ez25%3By29-%3Ewft%3Bx29-%3Ewft%3Brqd-%3Egrb%3Bcns-%3Egrb%3Bjbv-%3Evjg%3Bkjg-%3Evjg%3Btvm-%3Ez38%3Bpkw-%3Ez38%3Bvwd-%3Emgw%3Bkvv-%3Emgw%3Bnmw-%3Ez02%3Bbgp-%3Ez02%3Bx16-%3Emft%3By16-%3Emft%3Bx37-%3Ehbr%3By37-%3Ehbr%3Bx24-%3Ecns%3By24-%3Ecns%3By40-%3Epgc%3Bx40-%3Epgc%3Bccd-%3Ecbk%3Bcst-%3Ecbk%3Bfht-%3Ez20%3Bddw-%3Ez20%3Bwft-%3Estm%3Bnnq-%3Estm%3Bsrk-%3Efwg%3Bsjm-%3Efwg%3By06-%3Ecnm%3Bx06-%3Ecnm%3Bgqg-%3Ez07%3Bpmv-%3Ez07%3Bndp-%3Ergv%3Bnfq-%3Ergv%3Bbsm-%3Efjn%3Bvsc-%3Efjn%3Bx15-%3Esrk%3By15-%3Esrk%3Bbwv-%3Ez42%3Bvbq-%3Ez42%3Bkbv-%3Ez31%3Bctw-%3Ez31%3Bx10-%3Ehtc%3By10-%3Ehtc%3Bx08-%3Eknh%3By08-%3Eknh%3Bfgc-%3Enfn%3Bdvm-%3Enfn%3By03-%3Essj%3Bx03-%3Essj%3Bx26-%3Espf%3By26-%3Espf%3By02-%3Epsw%3Bx02-%3Epsw%3Bx36-%3Efcf%3By36-%3Efcf%3Bptj-%3Espg%3Bdtg-%3Espg%3Bstm-%3Egsj%3Btjt-%3Egsj%3Bmmw-%3Ez23%3Bncf-%3Ez23%3By26-%3Ecsh%3Bx26-%3Ecsh%3Brrj-%3Etnt%3Bmft-%3Etnt%3Bx05-%3Echs%3By05-%3Echs%3Bx16-%3Ekhn%3By16-%3Ekhn%3Bnqm-%3Etgq%3Bfmf-%3Etgq%3Bwft-%3Ez29%3Bnnq-%3Ez29%3By13-%3Eggg%3Bx13-%3Eggg%3Brcb-%3Emfp%3Bkhg-%3Emfp%3By28-%3Ewgd%3Bx28-%3Ewgd%3Bcsf-%3Eqsg%3Bbck-%3Eqsg%3Bx31-%3Ekbv%3By31-%3Ekbv%3Brrj-%3Ez16%3Bmft-%3Ez16%3Bhhg-%3Emsp%3Bccp-%3Emsp%3By00-%3Ez00%3Bx00-%3Ez00%3Bx25-%3Ekhg%3By25-%3Ekhg%3Bx42-%3Edjs%3By42-%3Edjs%3Bggg-%3Ecsf%3Bhvg-%3Ecsf%3Bpps-%3Evdc%3Bwdg-%3Evdc%3By04-%3Epvj%3Bx04-%3Epvj%3Bqtg-%3Eccd%3Bknh-%3Eccd%3Bx27-%3Ejns%3By27-%3Ejns%3Bjdk-%3Espk%3Bpmr-%3Espk%3Bnhm-%3Ez18%3Bjmr-%3Ez18%3Bfpk-%3Eccr%3Bdfh-%3Eccr%3Bx06-%3Ebgq%3By06-%3Ebgq%3Bx38-%3Edcc%3By38-%3Edcc%3Bx22-%3Ejkq%3By22-%3Ejkq%3Bvbq-%3Etgk%3Bbwv-%3Etgk%3Bx14-%3Ebck%3By14-%3Ebck%3Bfjn-%3Edtg%3Bhtc-%3Edtg%3Bkfd-%3Ez19%3Brtg-%3Ez19%3Bbvk-%3Enmw%3Bkhh-%3Enmw%3Bx34-%3Eptk%3By34-%3Eptk%3Bmmw-%3Erkf%3Bncf-%3Erkf%3By11-%3Ervg%3Bx11-%3Ervg%3Bwgd-%3Ennq%3Bvjg-%3Ennq%3Bqss-%3Ebwv%3Bwtt-%3Ebwv%3Bdmb-%3Eddw%3Bcrv-%3Eddw%3By08-%3Ecst%3Bx08-%3Ecst%3Bnhh-%3Ejmr%3Btgq-%3Ejmr%3Bcpt-%3Ez32%3Bfqc-%3Ez32%3Btnt-%3Efmf%3Bkhn-%3Efmf%3Bfbk-%3Ehvg%3Btcq-%3Ehvg%3Bspq-%3Ez01%3Bgpk-%3Ez01%3Bx02-%3Ebgp%3By02-%3Ebgp%3Bx20-%3Efct%3By20-%3Efct%3Bx36-%3Ejbq%3By36-%3Ejbq%3Bmfp-%3Ez26%3Bspf-%3Ez26%3Bspg-%3Ez11%3Bhhm-%3Ez11%3By32-%3Erjq%3Bx32-%3Erjq%3By04-%3Enfq%3Bx04-%3Enfq%3Bpkj-%3Ekvv%3Bmsp-%3Ekvv%3Btvc-%3Edvm%3Bdcc-%3Edvm%3Bvnr-%3Edbf%3Bkrb-%3Edbf%3Bfmf-%3Ez17%3Bnqm-%3Ez17%3Brtg-%3Edmb%3Bkfd-%3Edmb%3Bfvh-%3Ez40%3Bpgc-%3Ez40%3Bfcf-%3Ez36%3Bspk-%3Ez36%3Bpvc-%3Ekmm%3Bgst-%3Ekmm%3Bcbk-%3Evsc%3Bnbm-%3Evsc%3Bx09-%3Ebsm%3By09-%3Ebsm%3Bx07-%3Egqg%3By07-%3Egqg%3Bptk-%3Ez34%3Bkmm-%3Ez34%3Bx28-%3Ekjg%3By28-%3Ekjg%3Bdbm-%3Eqtg%3Bjft-%3Eqtg%3Bjbr-%3Ez33%3Bwcs-%3Ez33%3By42-%3Evbq%3Bx42-%3Evbq%3By00-%3Espq%3Bx00-%3Espq%3By19-%3Ekfd%3Bx19-%3Ekfd%3Btbn-%3Ejhd%3Bgrb-%3Ejhd%3Bkvv-%3Ez44%3Bvwd-%3Ez44%3Bwcs-%3Egst%3Bjbr-%3Egst%3Bsrk-%3Ez15%3Bsjm-%3Ez15%3By39-%3Ecbf%3Bx39-%3Ecbf%3Bbgq-%3Ez06%3Bwdh-%3Ez06%3Bbbn-%3Ecdc%3Brsc-%3Ecdc%3By18-%3Enhm%3Bx18-%3Enhm%3Btvm-%3Etvc%3Bpkw-%3Etvc%3Bx23-%3Encf%3By23-%3Encf%3Bx19-%3Ecrv%3By19-%3Ecrv%3Bqhg-%3Efqc%3Bgnc-%3Efqc%3Bjbv-%3Ez28%3Bkjg-%3Ez28%3By05-%3Enph%3Bx05-%3Enph%3Bx32-%3Ecpt%3By32-%3Ecpt%3Bx31-%3Egnc%3By31-%3Egnc%3Bccp-%3Ez43%3Bhhg-%3Ez43%3Bx15-%3Egtf%3By15-%3Egtf%3Bjmr-%3Eptd%3Bnhm-%3Eptd%3Bx10-%3Eptj%3By10-%3Eptj%3Bbgq-%3Edgh%3Bwdh-%3Edgh%3Bftw-%3Ez30%3Bgsj-%3Ez30%3Bx44-%3Ebds%3By44-%3Ebds%3Bfjn-%3Ez10%3Bhtc-%3Ez10%3By43-%3Epkj%3Bx43-%3Epkj%3By30-%3Enmj%3Bx30-%3Enmj%3Btvb-%3Ercb%3Bjhd-%3Ercb%3Bx40-%3Egdm%3By40-%3Egdm%3Bmgw-%3Ez45%3Bbds-%3Ez45%3Bfdv-%3Ewtt%3Bdfs-%3Ewtt%3Brkf-%3Erqd%3Bmhq-%3Erqd%3Bpvj-%3Ez04%3Bqfk-%3Ez04%3Bpmv-%3Edbm%3Bgqg-%3Edbm%3Bx21-%3Ebbn%3By21-%3Ebbn%3Bx38-%3Etvm%3By38-%3Etvm%3Bmjr-%3Epkw%3Bkpb-%3Epkw%3Bjbq-%3Eqfp%3Bvmj-%3Eqfp%3Bjkq-%3Ez22%3Bnhn-%3Ez22%3Bqfn-%3Ersc%3Bfct-%3Ersc%3Bx30-%3Eftw%3By30-%3Eftw%3By35-%3Edsb%3Bx35-%3Edsb%3By24-%3Etbn%3Bx24-%3Etbn%3Bfcf-%3Evmj%3Bspk-%3Evmj%3Bcnm-%3Epmv%3Bdgh-%3Epmv%3Bx27-%3Emcv%3By27-%3Emcv%3Bchs-%3Ez05%3Brgv-%3Ez05%3Bvdc-%3Efbk%3Bkcp-%3Efbk%3By39-%3Efgc%3Bx39-%3Efgc%3Bx13-%3Etcq%3By13-%3Etcq%3Bssj-%3Eqfk%3Bccr-%3Eqfk%3Bfwg-%3Errj%3Bgtf-%3Errj%3Bknh-%3Ez08%3Bqtg-%3Ez08%3Bfqc-%3Enfh%3Bcpt-%3Enfh%3By17-%3Enqm%3Bx17-%3Enqm%3Bhbr-%3Ez37%3Bqfp-%3Ez37%3By22-%3Enrq%3Bx22-%3Enrq%3By01-%3Egpk%3Bx01-%3Egpk%3Bjhv-%3Ewdh%3Bnph-%3Ewdh%3Bdfs-%3Ez41%3Bfdv-%3Ez41%3Bgqb-%3Embp%3Bcsh-%3Embp%3Bdvm-%3Ez39%3Bfgc-%3Ez39%3By03-%3Efpk%3Bx03-%3Efpk%3Bmfp-%3Egqb%3Bspf-%3Egqb%3Bx44-%3Evwd%3By44-%3Evwd%3Bgpk-%3Ebvk%3Bspq-%3Ebvk%3Bx21-%3Estq%3By21-%3Estq%3Bcbf-%3Efvh%3Bnfn-%3Efvh%3Bspg-%3Ewvv%3Bhhm-%3Ewvv%3Bcns-%3Ez24%3Brqd-%3Ez24%3Brjq-%3Ewcs%3Bnfh-%3Ewcs%3Bmcv-%3Enpn%3Bmbp-%3Enpn%3Bnrq-%3Emmw%3Bvkg-%3Emmw%3Bx33-%3Epvc%3By33-%3Epvc%3Bx20-%3Efht%3By20-%3Efht%3By43-%3Ehhg%3Bx43-%3Ehhg%3Bdfh-%3Ez03%3Bfpk-%3Ez03%3Bfbk-%3Ez13%3Btcq-%3Ez13%3Bx09-%3Enbm%3By09-%3Enbm%3Bpwp-%3Ectw%3Bnmj-%3Ectw%3By25-%3Etvb%3Bx25-%3Etvb%3By34-%3Ekrb%3Bx34-%3Ekrb%3Bx11-%3Ehhm%3By11-%3Ehhm%3Bhdq-%3Efdv%3Bgdm-%3Efdv%3Bmcv-%3Ez27%3Bmbp-%3Ez27%3Bnbm-%3Ez09%3Bcbk-%3Ez09%3Bfvh-%3Ehdq%3Bpgc-%3Ehdq%3Bjkq-%3Evkg%3Bnhn-%3Evkg%3Bdwt-%3Esjm%3Bqsg-%3Esjm%3Bdbf-%3Epmr%3Bdsb-%3Epmr%3By14-%3Edwt%3Bx14-%3Edwt%3Bx35-%3Ejdk%3By35-%3Ejdk%3Bx41-%3Edfs%3By41-%3Edfs%3By37-%3Ekpb%3Bx37-%3Ekpb%3Bbck-%3Ez14%3Bcsf-%3Ez14%3By07-%3Ejft%3Bx07-%3Ejft%3Bsdp-%3Ertg%3Bptd-%3Ertg%3By01-%3Ekhh%3Bx01-%3Ekhh%3Bbcr-%3Edfh%3Bpsw-%3Edfh%3Bchs-%3Ejhv%3Brgv-%3Ejhv%3By18-%3Esdp%3Bx18-%3Esdp%3Bddw-%3Eqfn%3Bfht-%3Eqfn%3Bx33-%3Ejbr%3By33-%3Ejbr%3Bgsj-%3Epwp%3Bftw-%3Epwp%3Bx12-%3Epps%3By12-%3Epps%3Brvg-%3Ewdg%3Bwvv-%3Ewdg%3B%20%20%0A%7D
;digraph G {
;           subgraph input_x {
;                                      node [style=filled,color=lightgrey];
;                                      x00 -> x01 -> x02 -> x03 -> x04 -> x05 -> x06 -> x07 -> x08 -> x09 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24 -> x25 -> x26 -> x27 -> x28 -> x29 -> x30 -> x31 -> x32 -> x33 -> x34 -> x35 -> x36 -> x37 -> x38 -> x39 -> x40 -> x41 -> x42 -> x43 -> x44 -> x45};
;
;           subgraph input_y {
;                                      node [style=filled,color=lightgrey];
;                                      y00 -> y01 -> y02 -> y03 -> y04 -> y05 -> y06 -> y07 -> y08 -> y09 -> y10 -> y11 -> y12 -> y13 -> y14 -> y15 -> y16 -> y17 -> y18 -> y19 -> y20 -> y21 -> y22 -> y23 -> y24 -> y25 -> y26 -> y27 -> y28 -> y29 -> y30 -> y31 -> y32 -> y33 -> y34 -> y35 -> y36 -> y37 -> y38 -> y39 -> y40 -> y41 -> y42 -> y43 -> y44 -> y45};
;
;           subgraph gates_and {
;                                        node [style=filled,color=lightgreen];
;                                        x23};y23;x41;y41;kbv;ctw;nmw;bgp;y29;x29;y17;x17;kmm;ptk;hbr;qfp;qfk;pvj;rqd;cns;jbv;kjg;vwd;kvv;wft;nnq;srk;sjm;y06;x06;fgc;dvm;y03;x03;y02;x02;y26;x26;rrj;mft;x16;y16;nqm;fmf;y13;x13;y28;x28;csf;bck;hhg;ccp;x42;y42;qtg;knh;x27;y27;fpk;dfh;x38;y38;vbq;bwv;fjn;htc;mmw;ncf;y11;x11;y08;x08;fbk;tcq;x20;y20;x36;y36;x12;y12;y32;x32;y04;x04;rtg;kfd;cbk;nbm;x09;y09;jbr;wcs;y00;x00;y39;x39;bbn;rsc;tvm;pkw;x19;y19;y05;x05;x31;y31;x15;y15;jmr;nhm;x10;y10;bgq;wdh;x44;y44;y43;x43;y30;x30;tvb;jhd;x40;y40;fdv;dfs;pmv;gqg;y24;x24;fcf;spk;fqc;cpt;y22;x22;mfp;spf;gpk;spq;x21;y21;spg;hhm;mcv;mbp;wdg;pps;x33;y33;y25;x25;y34;x34;fvh;pgc;jkq;nhn;dbf;dsb;y14;x14;x35;y35;y37;x37;y07;x07;y01;x01;chs;rgv;y18;x18;ddw;fht;gsj;ftw;
;
;           subgraph gates_or {
;                                       node [style=filled,color=yellow];
;                                       djs};tgk;jns;npn;cdc;stq;ccd;cst;ndp;nfq;bsm;vsc;ptj;dtg;stm;tjt;rcb;khg;ggg;hvg;jdk;pmr;bvk;khh;wgd;vjg;qss;wtt;dmb;crv;nhh;tgq;tnt;khn;pkj;msp;tvc;dcc;vnr;krb;pvc;gst;dbm;jft;tbn;grb;qhg;gnc;mgw;bds;rkf;mhq;mjr;kpb;jbq;vmj;qfn;fct;cnm;dgh;vdc;kcp;ssj;ccr;fwg;gtf;jhv;nph;gqb;csh;cbf;nfn;rjq;nfh;nrq;vkg;pwp;nmj;hdq;gdm;dwt;qsg;sdp;ptd;bcr;psw;rvg;wvv;
;
;           subgraph gates_xor {
;                                        node [style=filled,color=lightskyblue];
;                                        dbf};dsb;tvb;jhd;y29;x29;tvm;pkw;nmw;bgp;x16;y16;x37;y37;x24;y24;y40;x40;fht;ddw;gqg;pmv;x15;y15;bwv;vbq;kbv;ctw;x10;y10;x08;y08;x26;y26;x36;y36;mmw;ncf;x05;y05;wft;nnq;x31;y31;rrj;mft;y00;x00;x25;y25;pps;wdg;y04;x04;nhm;jmr;x06;y06;x22;y22;x14;y14;kfd;rtg;x34;y34;cpt;fqc;spq;gpk;x02;y02;mfp;spf;spg;hhm;fmf;nqm;fvh;pgc;fcf;spk;x07;y07;ptk;kmm;x28;y28;y42;x42;y19;x19;kvv;vwd;wcs;jbr;srk;sjm;bgq;wdh;y18;x18;x23;y23;jbv;kjg;x32;y32;ccp;hhg;ftw;gsj;fjn;htc;pvj;qfk;x21;y21;x38;y38;jkq;nhn;x30;y30;y35;x35;x27;y27;chs;rgv;y39;x39;x13;y13;knh;qtg;y17;x17;hbr;qfp;y01;x01;dfs;fdv;dvm;fgc;y03;x03;x44;y44;cns;rqd;x20;y20;y43;x43;dfh;fpk;fbk;tcq;x09;y09;x11;y11;mcv;mbp;nbm;cbk;rsc;bbn;x41;y41;bck;csf;x33;y33;x12;y12;
;
;
;           subgraph output_z {
;                                       node [style=filled,color=lightgreen];
;                                       z00 -> z01 -> z02 -> z03 -> z04 -> z05 -> z06 -> z07 -> z08 -> z09 -> z10 -> z11 -> z12 -> z13 -> z14 -> z15 -> z16 -> z17 -> z18 -> z19 -> z20 -> z21 -> z22 -> z23 -> z24 -> z25 -> z26 -> z27 -> z28 -> z29 -> z30 -> z31 -> z32 -> z33 -> z34 -> z35 -> z36 -> z37 -> z38 -> z39 -> z40 -> z41 -> z42 -> z43 -> z44 -> z45};
;
;
;
;           rsc->z21};bbn->z21;cdc->nhn;stq->nhn;x12->kcp;y12->kcp;wdg->z12;pps->z12;x23->mhq;y23->mhq;dbf->z35;dsb->z35;x41->qss;y41->qss;kbv->qhg;ctw->qhg;djs->ccp;tgk->ccp;nmw->bcr;bgp->bcr;jns->jbv;npn->jbv;y29->tjt;x29->tjt;y17->nhh;x17->nhh;kmm->vnr;ptk->vnr;hbr->mjr;qfp->mjr;qfk->ndp;pvj->ndp;tvb->z25;jhd->z25;y29->wft;x29->wft;rqd->grb;cns->grb;jbv->vjg;kjg->vjg;tvm->z38;pkw->z38;vwd->mgw;kvv->mgw;nmw->z02;bgp->z02;x16->mft;y16->mft;x37->hbr;y37->hbr;x24->cns;y24->cns;y40->pgc;x40->pgc;ccd->cbk;cst->cbk;fht->z20;ddw->z20;wft->stm;nnq->stm;srk->fwg;sjm->fwg;y06->cnm;x06->cnm;gqg->z07;pmv->z07;ndp->rgv;nfq->rgv;bsm->fjn;vsc->fjn;x15->srk;y15->srk;bwv->z42;vbq->z42;kbv->z31;ctw->z31;x10->htc;y10->htc;x08->knh;y08->knh;fgc->nfn;dvm->nfn;y03->ssj;x03->ssj;x26->spf;y26->spf;y02->psw;x02->psw;x36->fcf;y36->fcf;ptj->spg;dtg->spg;stm->gsj;tjt->gsj;mmw->z23;ncf->z23;y26->csh;x26->csh;rrj->tnt;mft->tnt;x05->chs;y05->chs;x16->khn;y16->khn;nqm->tgq;fmf->tgq;wft->z29;nnq->z29;y13->ggg;x13->ggg;rcb->mfp;khg->mfp;y28->wgd;x28->wgd;csf->qsg;bck->qsg;x31->kbv;y31->kbv;rrj->z16;mft->z16;hhg->msp;ccp->msp;y00->z00;x00->z00;x25->khg;y25->khg;x42->djs;y42->djs;ggg->csf;hvg->csf;pps->vdc;wdg->vdc;y04->pvj;x04->pvj;qtg->ccd;knh->ccd;x27->jns;y27->jns;jdk->spk;pmr->spk;nhm->z18;jmr->z18;fpk->ccr;dfh->ccr;x06->bgq;y06->bgq;x38->dcc;y38->dcc;x22->jkq;y22->jkq;vbq->tgk;bwv->tgk;x14->bck;y14->bck;fjn->dtg;htc->dtg;kfd->z19;rtg->z19;bvk->nmw;khh->nmw;x34->ptk;y34->ptk;mmw->rkf;ncf->rkf;y11->rvg;x11->rvg;wgd->nnq;vjg->nnq;qss->bwv;wtt->bwv;dmb->ddw;crv->ddw;y08->cst;x08->cst;nhh->jmr;tgq->jmr;cpt->z32;fqc->z32;tnt->fmf;khn->fmf;fbk->hvg;tcq->hvg;spq->z01;gpk->z01;x02->bgp;y02->bgp;x20->fct;y20->fct;x36->jbq;y36->jbq;mfp->z26;spf->z26;spg->z11;hhm->z11;y32->rjq;x32->rjq;y04->nfq;x04->nfq;pkj->kvv;msp->kvv;tvc->dvm;dcc->dvm;vnr->dbf;krb->dbf;fmf->z17;nqm->z17;rtg->dmb;kfd->dmb;fvh->z40;pgc->z40;fcf->z36;spk->z36;pvc->kmm;gst->kmm;cbk->vsc;nbm->vsc;x09->bsm;y09->bsm;x07->gqg;y07->gqg;ptk->z34;kmm->z34;x28->kjg;y28->kjg;dbm->qtg;jft->qtg;jbr->z33;wcs->z33;y42->vbq;x42->vbq;y00->spq;x00->spq;y19->kfd;x19->kfd;tbn->jhd;grb->jhd;kvv->z44;vwd->z44;wcs->gst;jbr->gst;srk->z15;sjm->z15;y39->cbf;x39->cbf;bgq->z06;wdh->z06;bbn->cdc;rsc->cdc;y18->nhm;x18->nhm;tvm->tvc;pkw->tvc;x23->ncf;y23->ncf;x19->crv;y19->crv;qhg->fqc;gnc->fqc;jbv->z28;kjg->z28;y05->nph;x05->nph;x32->cpt;y32->cpt;x31->gnc;y31->gnc;ccp->z43;hhg->z43;x15->gtf;y15->gtf;jmr->ptd;nhm->ptd;x10->ptj;y10->ptj;bgq->dgh;wdh->dgh;ftw->z30;gsj->z30;x44->bds;y44->bds;fjn->z10;htc->z10;y43->pkj;x43->pkj;y30->nmj;x30->nmj;tvb->rcb;jhd->rcb;x40->gdm;y40->gdm;mgw->z45;bds->z45;fdv->wtt;dfs->wtt;rkf->rqd;mhq->rqd;pvj->z04;qfk->z04;pmv->dbm;gqg->dbm;x21->bbn;y21->bbn;x38->tvm;y38->tvm;mjr->pkw;kpb->pkw;jbq->qfp;vmj->qfp;jkq->z22;nhn->z22;qfn->rsc;fct->rsc;x30->ftw;y30->ftw;y35->dsb;x35->dsb;y24->tbn;x24->tbn;fcf->vmj;spk->vmj;cnm->pmv;dgh->pmv;x27->mcv;y27->mcv;chs->z05;rgv->z05;vdc->fbk;kcp->fbk;y39->fgc;x39->fgc;x13->tcq;y13->tcq;ssj->qfk;ccr->qfk;fwg->rrj;gtf->rrj;knh->z08;qtg->z08;fqc->nfh;cpt->nfh;y17->nqm;x17->nqm;hbr->z37;qfp->z37;y22->nrq;x22->nrq;y01->gpk;x01->gpk;jhv->wdh;nph->wdh;dfs->z41;fdv->z41;gqb->mbp;csh->mbp;dvm->z39;fgc->z39;y03->fpk;x03->fpk;mfp->gqb;spf->gqb;x44->vwd;y44->vwd;gpk->bvk;spq->bvk;x21->stq;y21->stq;cbf->fvh;nfn->fvh;spg->wvv;hhm->wvv;cns->z24;rqd->z24;rjq->wcs;nfh->wcs;mcv->npn;mbp->npn;nrq->mmw;vkg->mmw;x33->pvc;y33->pvc;x20->fht;y20->fht;y43->hhg;x43->hhg;dfh->z03;fpk->z03;fbk->z13;tcq->z13;x09->nbm;y09->nbm;pwp->ctw;nmj->ctw;y25->tvb;x25->tvb;y34->krb;x34->krb;x11->hhm;y11->hhm;hdq->fdv;gdm->fdv;mcv->z27;mbp->z27;nbm->z09;cbk->z09;fvh->hdq;pgc->hdq;jkq->vkg;nhn->vkg;dwt->sjm;qsg->sjm;dbf->pmr;dsb->pmr;y14->dwt;x14->dwt;x35->jdk;y35->jdk;x41->dfs;y41->dfs;y37->kpb;x37->kpb;bck->z14;csf->z14;y07->jft;x07->jft;sdp->rtg;ptd->rtg;y01->khh;x01->khh;bcr->dfh;psw->dfh;chs->jhv;rgv->jhv;y18->sdp;x18->sdp;ddw->qfn;fht->qfn;x33->jbr;y33->jbr;gsj->pwp;ftw->pwp;x12->pps;y12->pps;rvg->wdg;wvv->wdg;
;