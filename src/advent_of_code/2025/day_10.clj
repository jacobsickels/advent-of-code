(ns advent-of-code.2025.day-10
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2025/day_10.txt")))

(defn parse-line [line]
  (let [lights-string (second (re-find #"\[(.*?)\]" line))
        buttons-string (str/trim (second (re-find #"\](.*?)\{" line)))]
    {:lights  lights-string
     :buttons (->> (re-seq #"\((.*?)\)" buttons-string)
                   (map first)
                   (map read-string))
     :joltage (->> (str/split (second (re-find #"\{(.*?)\}" line)) #",")
                   (map #(Integer/parseInt %)))}))

(def parsed-data (map parse-line data))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn button-to-string [lights-cnt button]
  (loop [b button
         template (apply str (repeat lights-cnt "."))]
    (if (empty? b)
      template
      (recur (rest b) (replace-at template (first b) "#")))))

(defn press-button [input-string button]
  (apply str (map
               (fn [input button]
                 (cond
                   (= input \.)
                   (if (= button \#) \# \.)

                   (= input \#)
                   (if (= button \#) \. \#)))
               input-string (button-to-string (count input-string) button))))

(defn press-buttons [input-string buttons]
  (map #(press-button input-string %) buttons))

(defn presses-contain-input? [presses input]
  (contains? presses input))

(defn press-buttons-for-inputs [inputs buttons]
  (mapcat #(press-buttons % buttons) inputs))

(defn bfs-to-input [input buttons]
  (loop [visited #{(apply str (repeat (count input) "."))}
         ittr 0]
    (if (presses-contain-input? visited input)
      ittr
      (let [next-presses (set (press-buttons-for-inputs visited buttons))]
        (recur (set/union visited next-presses)
               (inc ittr))))))

(defn part-1 []
  (->> (map #(bfs-to-input (:lights %) (:buttons %)) parsed-data)
       (reduce +)))

;; =========================== Not finished
;; Looks like a system of equations but not sure how to solve

(defn button-to-joltage [joltage-input button]
  (loop [b button
         template (vec (repeat (count joltage-input) 0))]
    (if (empty? b)
      template
      (recur (rest b) (assoc template (first b) 1)))))

(use 'clocop.core
     'clocop.constraints)

(defn testing-out-clocop []
  (with-store (store)                                       ; initialize the variable store
              (let [a (int-var "a" 0 1000)                  ;; [3]
                    b (int-var "b" 0 1000)                  ;; [1,3]
                    c (int-var "c" 0 1000)                  ;; [2]
                    d (int-var "d" 0 1000)                  ;; [2,3]
                    e (int-var "e" 0 1000)                  ;; [0,2]
                    f (int-var "f" 0 1000)                  ;; [0,1]]
                    w (int-var "w" 3)
                    x (int-var "x" 5)
                    y (int-var "y" 4)
                    z (int-var "z" 7)]
                ; setting constraints
                (constrain! ($= ($+ e f) w))
                (constrain! ($= ($+ b f) x))
                (constrain! ($= ($+ c d e) y))
                (constrain! ($= ($+ a b d) z))
                (solve!))))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn get-solution-value [solution]
  (->> (map #(get solution (str %)) alphabet)
       (take-while #(not (nil? %)))
       (reduce +)))
(defn solver [entry]
  (let [solutions (with-store (store)
                              (let [button-ints (vec (map-indexed (fn [idx btn] {:btn btn :variable (int-var (str (get alphabet idx)) 0 1000)}) (:buttons entry)))
                                    constraints (vec (map-indexed (fn [idx jolt] {:jolt-index idx :variable (int-var (str (nth (reverse alphabet) idx)) jolt)}) (:joltage entry)))
                                    _ (vec (map (fn [constraint]
                                                  (let [constraint-vars (->> (filter (fn [btn] (contains? (set (:btn btn)) (:jolt-index constraint))) button-ints)
                                                                             (mapv :variable))]
                                                    (constrain! ($= (:variable constraint) (if (= 1 (count constraint-vars))
                                                                                             (first constraint-vars)
                                                                                             (apply $+ constraint-vars))))))
                                                constraints))]
                                (solve! :solutions :all)))]
    (->> (map get-solution-value solutions)
         (sort)
         (first))))

(defn part-2 []
  (->> (map #(solver %) parsed-data)
       (reduce +)))













