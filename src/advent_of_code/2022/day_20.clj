(ns advent-of-code.2022.day-20
  (:require [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "2022/day_20.edn"))

(defn make-circle [col]
  (loop [pointer 0
         acc {}]
    (cond
      (>= pointer (count col))
      acc

      (= pointer (dec (count col)))
      (recur (inc pointer)
             (assoc acc (nth col pointer) [(nth col (dec pointer)) (nth col 0)]))

      (empty? acc)
      (recur (inc pointer)
             (assoc acc (nth col pointer) [(last col) (nth col (inc pointer))]))

      :else
      (recur (inc pointer)
             (assoc acc (nth col pointer) [(nth col (dec pointer)) (nth col (inc pointer))])))))


(defn get-circle-number [circle number]
  (let [direction (if (pos? (second number)) second first)]
    (loop [amount (Math/abs (second number))
           found number]

      (cond
        (zero? amount) found

        ;;; I've looped around to myself?
        ;; I don't want myself to count as a space (probably the issue?)
        (and (< amount (Math/abs (second number))) (= found number))
        (recur amount (direction (get circle found)))

        :else (recur (dec amount) (direction (get circle found)))))))

(defn remove-from-circle [circle number]
  (let [[old-l old-r] (get circle number)
        [ll _] (get circle old-l)
        [_ rr] (get circle old-r)]
    (-> (dissoc circle number)
        (assoc old-r [old-l rr]
               old-l [ll old-r]))))

(defn add-to-circle [circle number to]
  (let [[old-l old-r] (get circle to)
        [ll _] (get circle old-l)
        [_ rr] (get circle old-r)]
    (if (pos? (second number))
      (assoc circle to [old-l number]
                    number [to old-r]
                    old-r [number rr])
      (assoc circle to [number old-r]
                    number [old-l to]
                    old-l [ll number]))))

(defn move-number-amount [circle number]
  (if (zero? (second number))
    circle
    (let [to (get-circle-number circle number)]
      (-> (remove-from-circle circle number)
          (add-to-circle number to)))))

(defn do-mixing [col]
  (let [indexed-col (map-indexed (fn [i n] (list i n)) col)]
    (loop [check indexed-col
           circle (make-circle indexed-col)]
      (if (empty? check)
        circle
        (recur (rest check)
               (move-number-amount circle (first check)))))))

(defn get-number-amt-ahead [circle start amt-ahead]
  (loop [amount amt-ahead
         found start]
    (cond
      (zero? amount) found

      :else (recur (dec amount) (second (get circle found))))))

(defn part-1 [col]
  (let [circle (do-mixing col)
        start (->> (map-indexed (fn [i n] (list i n)) col)
                   (filter #(zero? (second %)))
                   (first))
        f (get-number-amt-ahead circle start 1000)
        s (get-number-amt-ahead circle start 2000)
        t (get-number-amt-ahead circle start 3000)]
    [circle (->> (map second [f s t])
                 (reduce +))]))