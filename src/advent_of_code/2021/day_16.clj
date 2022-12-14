(ns advent-of-code.2021.day-16
  (:require [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "day_16.edn"))

(defn hex-to-binary [hex]
  (apply str (map #(let [binary-str (Long/toBinaryString (Long/parseLong % 16))]
                     (apply str (flatten [(repeat (- 4 (count binary-str)) "0") binary-str])))
                  (map str hex))))

(defn get-standard-header [hex]
  (let [binary (hex-to-binary hex)]
    {:version (Integer/parseInt (apply str (take 3 binary)) 2)
     :id (Integer/parseInt (apply str (take 3 (drop 3 (hex-to-binary hex)))) 2)}))

(defn is-literal-packet? [hex]
  (= (:id (get-standard-header hex)) 4))

(defn parse-literal-packet [hex]
  (let [headers (get-standard-header hex)
        ;; Dropping headers off to calculate
        binary (drop 6 (hex-to-binary hex))
        [a b c] (partition 5 binary)]
    (merge headers 
           {:bits [(rest a) (rest b) (rest c)]
            :value (Integer/parseInt (apply str (concat (rest a) 
                                                 (rest b) 
                                                 (rest c)))
                                     2)})))

(defn parse-operator-packet [hex]
  (let [headers (get-standard-header hex)
        ;; Dropping headers off to calculate
        binary (drop 6 (hex-to-binary hex))
        length? (= \0 (first (take 1 binary)))]
    (merge headers 
           {:length? length?
            :value (Integer/parseInt (apply str (take (if length? 15 11) (drop 1 binary)))
                                     2)})))

(defn parse-packet [hex]
 (loop [binary (hex-to-binary hex)
        acc {}]
   (cond 
     (nil? (:version acc))
     (recur (drop 3 binary) (assoc acc :version (Integer/parseInt (apply str (take 3 binary)) 2)))
     
     (nil? (:id acc))
     (recur (drop 3 binary) (assoc acc :id (Integer/parseInt (apply str (take 3 binary)) 2)))
     
     (and (not= (:id acc) 4) (nil? (:length-type acc)))
     ;; get length type next iteration start parsing sub packets
     
     :else 
     acc)))
        
    