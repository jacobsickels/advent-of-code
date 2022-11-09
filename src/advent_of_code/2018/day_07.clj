(ns advent-of-code.2018.day-07
  (:require [advent-of-code.shared.read-file :as read]))

(def test-data ["Step C must be finished before step A can begin."
                "Step C must be finished before step F can begin."
                "Step A must be finished before step B can begin."
                "Step A must be finished before step D can begin."
                "Step B must be finished before step E can begin."
                "Step D must be finished before step E can begin."
                "Step F must be finished before step E can begin."])

(def data (read/read-file "resources/2018/day_07.txt"))

(defn get-data [col]
  (map
    (fn [item]
      (let [pre (last (re-find #"Step \S" item))
            to (last (re-find #"step \S" item))]
        (vector pre to)))
    col))

(defn- has-dependencies? [c col]
  (seq? (seq (filter #(= (second %) c) col))))

(defn get-characters [col]
  (sort (set (flatten col))))

(defn- find-first-no-deps [current-chars col]
  (loop [characters current-chars]
    (if (not (has-dependencies? (first characters) col))
      (first characters)
      (recur (rest characters)))))

(defn remove-character-no-deps [character col]
  (remove #(= (first %) character) col))

(defn part-1 [col]
  (loop [data (get-data col)
         characters (get-characters data)
         found ""]
    (if (empty? characters)
      found
      (let [next-found (find-first-no-deps characters data)]
        (recur
          (remove-character-no-deps next-found data)
          (remove #(= % next-found) characters)
          (str found next-found))))))


(defn remove-all-character-no-deps [characters col]
  (remove #(contains? (set characters) (first %)) col))

(defn work-amount [col augment]
  (into {} (map-indexed (fn [i c] (vector c (+ i augment))) (get-characters col))))

(defn- get-available-work [workers characters col]
  (let [already-being-done (set (map first workers))]
    (filter (fn [ch] (not (has-dependencies? ch col)))
            (remove #(contains? already-being-done %) characters))))

(defn get-work-amounts [workers col augment]
  (let [initial-data (get-data col)
        work-amt (work-amount initial-data augment)]
    (loop [data initial-data
           characters (get-characters data)
           work-amounts []]
      (if (empty? characters)
        (partition 2 (flatten work-amounts))
        (let [no-deps (get-available-work workers characters data)
              work (map #(vector % (get work-amt %)) no-deps)]
          (recur (remove-all-character-no-deps no-deps data)
                 (remove #(contains? (set no-deps) %) characters)
                 (conj work-amounts work)))))))

(defn- can-do-work? [workers]
  (some empty? workers))

;(defn part-2 [col]
;  (let [initial-data (get-data col)]
;    (loop [data initial-data
;           characters (get-characters initial-data)
;           work (get-work-amounts col)
;           workers (repeat 2 [])
;           iterations 0]
;      [characters work workers]
;      (if (empty? characters)
;        iterations
;        (let [available-work (get-available-work characters data)]
;          (if (and available-work (can-do-work? workers))
;            (recur ())))))))

;; (defn do-work [worker]
(defn tick-work [worker]
  (if (empty? worker)
    []
    [(first worker) (dec (second worker))]))

(defn do-work [workers]
  (map tick-work workers))

(defn is-work-done? [worker]
  (zero? (second worker)))

(defn- give-single-work-to-workers [workers available-single-work]
  (first (reduce (fn [[acc given] worker]
                   (if (and (not given) (empty? worker))
                     [(conj acc available-single-work) true]
                     [(conj acc worker) given]))
                 [[] false]
                 workers)))

(defn give-all-work-to-workers [work workers available]
  (map vec (reduce (fn [acc av-work]
                     (give-single-work-to-workers acc av-work))
                   workers
                   (filter #(contains? (set available) (first %)) work))))

(defn- worker-can-do-work? [worker]
  (empty? worker))

(defn open-workers? [workers]
  (some worker-can-do-work? workers))

(defn- worker-finished-work? [workers]
  (some is-work-done? (remove empty? workers)))

(defn- clear-finished-workers [workers]
  (map (fn [worker]
         (if (or (empty? worker) (zero? (second worker)))
           []
           worker))
       workers))

(defn- get-finished-work [workers]
  (->> (remove empty? workers)
       (filter (fn [worker] (zero? (second worker))))
       (map first)))

(defn remove-finished-work-from-data [finished-work data]
  (remove #(contains? (set finished-work) (first %)) data))

(defn part-2 [col worker-count augment]
  (let [initial-workers (repeat worker-count [])]
    (loop [data (get-data col)
           work (get-work-amounts initial-workers col augment)
           workers (give-all-work-to-workers
                     work
                     initial-workers
                     (get-available-work initial-workers (map first work) data))
           iterations 0]

      (cond
        (empty? work)
        iterations

        (worker-finished-work? workers)
        (let [cleared-workers (clear-finished-workers workers)
              finished-work (get-finished-work workers)
              new-work (remove #(contains? (set finished-work) (first %)) work)
              new-data (remove-finished-work-from-data finished-work data)]
          (recur new-data
                 new-work
                 (give-all-work-to-workers
                   new-work
                   (do-work cleared-workers)
                   (get-available-work workers (map first new-work) new-data))
                 (inc iterations)))

        :else
        (recur
          data
          work
          (do-work workers)
          (inc iterations))))))


