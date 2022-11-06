(ns advent-of-code.2018.day-05
  (:require [advent-of-code.shared.read-file :as read]))

(defn- should-react? [a b]
  (and
    (not (nil? a))
    (not (nil? b))
    (or (and (Character/isUpperCase a) (Character/isLowerCase b))
        (and (Character/isUpperCase b) (Character/isLowerCase a)))
    (= (Character/toLowerCase a) (Character/toLowerCase b))))

(defn- react-for-count [col]
  (loop [data col
         units []]
    (if (empty? data)
      (count units)
      (let [top-data (first data)
            top-units (first units)]
        (if (should-react? top-data top-units)
          (recur (rest data) (rest units))
          (recur (rest data) (concat [top-data] units)))))))

(defn part-1 []
  (react-for-count (seq (first (read/read-file "resources/2018/day_05.txt")))))

(defn- remove-all-of-type [type col]
  (remove #(= (Character/toLowerCase %) type) col))

(def types-to-remove #{\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z})

(defn part-2 []
  (let [data (seq (first (read/read-file "resources/2018/day_05.txt")))]
    (loop [types types-to-remove
           counts {}]
      (if (empty? types)
        counts
        (recur (rest types)
               (merge counts {(first types)
                              (react-for-count (remove-all-of-type (first types) data))}))))))