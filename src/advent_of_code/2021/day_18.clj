(ns advent-of-code.2021.day-18
  (:require [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "day_18.edn"))

(defn add_snail_numbers [left right]
  [left right])

(defn count-depth
  ([snail_number]
   (count-depth snail_number 0))
  ([snail_number depth]
   (if (number? snail_number)
     depth
     (let [[left right] snail_number]
       [(count-depth left (inc depth))
        (count-depth right (inc depth))]))))

(defn must-explode? [snail_number]
  (contains? (set (flatten (count-depth snail_number))) 5))


(defn make-depths
  ([snail_number]
   (make-depths snail_number 0))
  ([snail_number depth]
   {:depth depth 
    :left (if (coll? snail_number)
            (if (first snail_number) 
              (make-depths (first snail_number) (inc depth))
              snail_number)
            snail_number)
    :right (if (coll? snail_number) 
             (if (coll? (second snail_number)) 
               (make-depths (second snail_number) (inc depth))
               (second snail_number))
             snail_number)}))


(defn make-snail-array [snail_number])
  
  
  
  
  


;(defn reduce_snail_number 
;  ([[left right]]
;   (reduce_snail_number [left right] 1))
;  ([[left right] depth]
;   (println left right depth)
;   (cond
;     (= depth 4)
;     [true left right]
;     
;     :else
;     (let [[exploded? left right] (reduce_snail_number [left right] depth)]
;       (if exploded?)))))
;         
       
                
         
     
     