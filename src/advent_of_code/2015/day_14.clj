(ns advent-of-code.2015.day-14)

(def test-data [{:deer :comet :speed 14 :time 10 :wait 127}
                {:deer :dancer :speed 16 :time 11 :wait 162}])

(def data [{:deer :dancer :speed 27 :time 5 :wait 132}
           {:deer :cupid :speed 22 :time 2 :wait 41}
           {:deer :rudolph :speed 11 :time 5 :wait 48}
           {:deer :donner :speed 28 :time 5 :wait 134}
           {:deer :dasher :speed 4 :time 16 :wait 55}
           {:deer :blitzen :speed 14 :time 3 :wait 38}
           {:deer :prancer :speed 3 :time 21 :wait 40}
           {:deer :comet :speed 18 :time 6 :wait 103}
           {:deer :vixen :speed 18 :time 5 :wait 84}])

(defn deer-distance [deer time]
  (loop [seconds 0
         d deer
         distance 0]
    (if (> seconds time)
      distance
      (cond
        (and (zero? (:time d)) (zero? (:wait d)))
        (recur seconds deer distance)

        (zero? (:time d))
        (recur (inc seconds) (assoc d :wait (dec (:wait d))) distance)

        :else
        (recur (inc seconds) (assoc d :time (dec (:time d))) (+ distance (:speed deer)))))))

(defn reindeer-distances [col time]
  (loop [deer col
         acc {}]
    (if (empty? deer)
      acc
      (recur (rest deer) (assoc acc (:deer (first deer)) (deer-distance (first deer) time))))))

(defn part-1 []
  (apply max-key val (reindeer-distances data 2503)))

(defn get-leading-deer [col time]
  (let [distances (reindeer-distances col time)
        [_ max-value] (apply max-key val distances)]
      (filter (fn [[_ v]] (= v max-value)) distances)))

(defn increment-scores [scores leading-deer-col]
  (reduce (fn [acc deer]
            (assoc acc (first deer) (inc (or (get acc (first deer)) 0))))
          scores
          leading-deer-col))

(defn part-2 [col time]
  (loop [seconds 0
         scores {}]
    (if (= seconds time)
      (apply max-key val scores)
      (let [leading-deer (get-leading-deer col seconds)]
        (recur (inc seconds) (increment-scores scores leading-deer))))))