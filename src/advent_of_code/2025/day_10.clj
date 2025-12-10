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

(defn button-to-joltage [joltage-input button]
  (loop [b button
         template (vec (repeat (count joltage-input) 0))]
    (if (empty? b)
      template
      (recur (rest b) (assoc template (first b) 1)))))

(defn press-joltage-button [joltage-input button]
  (map (fn [a b] (+ a b)) joltage-input (button-to-joltage joltage-input button)))

(defn press-joltage-buttons [joltage-input buttons]
  (map #(press-joltage-button joltage-input %) buttons))

(defn press-buttons-for-joltages [joltage-inputs buttons]
  (mapcat #(press-joltage-buttons % buttons) joltage-inputs))

(defn bfs-to-input-2 [input buttons]
  (loop [visited #{(repeat (count input) 0)}
         ittr 0]
    (if (presses-contain-input? visited input)
      ittr
      (let [next-presses (set (press-buttons-for-joltages visited buttons))]
        (recur (set/union visited next-presses)
               (inc ittr))))))


(defn part-2 []
  (->> (map #(bfs-to-input-2 (:joltage %) (:buttons %)) parsed-data)
       (reduce +)))




















