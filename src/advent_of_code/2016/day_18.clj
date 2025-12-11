(ns advent-of-code.2016.day-18)

(defn is-trap? [left center right]
  (or (and left center (not right))
      (and (not left) center right)
      (and left (not center) (not right))
      (and (not left) (not center) right)))

(def example-data (map #(= \^ %) ".^^.^.^^^^"))
(def data (map #(= \^ %) ".^^^^^.^^.^^^.^...^..^^.^.^..^^^^^^^^^^..^...^^.^..^^^^..^^^^...^.^.^^^^^^^^....^..^^^^^^.^^^.^^^.^^"))

(defn get-next-row [input-row]
  (map-indexed (fn [idx _] (is-trap? (nth input-row (dec idx) false)
                                     (nth input-row idx false)
                                     (nth input-row (inc idx) false)))
               input-row))
(def memoized-get-next-row (memoize get-next-row))

(defn make-map [input length]
  (loop [row input
         freq (frequencies input)
         ittr 1]
    (if (= length ittr)
      freq
      (let [next-row (memoized-get-next-row row)]
        (recur next-row (merge-with + freq (frequencies next-row)) (inc ittr))))))


(defn example-1 [] (make-map example-data 10))
(defn part-1 [] (make-map data 40))
(defn part-2 [] (make-map data 400000))