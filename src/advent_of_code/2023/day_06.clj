(ns advent-of-code.2023.day-06)

(def test-data [[7 9]
                [15 40]
                [30 200]])

(def data [[41 249]
           [77 1362]
           [70 1127]
           [96 1011]])

(defn beats-record [start total distance]
  (> (* start (- total start)) distance))

(defn find-range [[time distance]]
  (loop [[start end] [0 time]]
    (cond
      (and (beats-record start time distance)
           (beats-record end time distance))
      [start end]

      (not (beats-record start time distance))
      (if (not (beats-record (+ start (quot end 4)) time distance))
        (recur [(+ start (quot end 4)) end])
        (recur [(inc start) end]))

      (not (beats-record end time distance))
      (if (not (beats-record (- end (quot end 4)) time distance))
        (recur [start (- end (quot end 4))])
        (recur [start (dec end)])))))

(defn part-1 [col]
  (->> (map find-range col)
       (map (fn [[start end]] (inc (- end start))))
       (reduce *)))

(def test-data-2 [71530 940200])

(def data-2 [41777096 249136211271011])

(defn part-2 [col]
  (let [[start end] (find-range col)]
    (inc (- end start))))