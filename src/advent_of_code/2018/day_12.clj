(ns advent-of-code.2018.day-12)


(def test-initial-state "#..#.#..##......###...###")

(def test-rules {"...##" "#"
                 "..#.." "#"
                 ".#..." "#"
                 ".#.#." "#"
                 ".#.##" "#"
                 ".##.." "#"
                 ".####" "#"
                 "#.#.#" "#"
                 "#.###" "#"
                 "##.#." "#"
                 "##.##" "#"
                 "###.." "#"
                 "###.#" "#"
                 "####." "#"})

(def initial-state "##...#......##......#.####.##.#..#..####.#.######.##..#.####...##....#.#.####.####.#..#.######.##...")

(def rules {"#...." "."
            "#..##" "#"
            "....#" "."
            "...#." "."
            "...##" "#"
            "#.#.#" "."
            ".#..." "#"
            "##.#." "."
            "..#.#" "."
            ".##.#" "#"
            "###.#" "#"
            ".#.##" "."
            "....." "."
            "#####" "#"
            "###.." "."
            "##..#" "#"
            "#.###" "#"
            "#.#.." "."
            "..###" "."
            "..#.." "."
            ".#..#" "#"
            ".##.." "#"
            "##..." "#"
            ".#.#." "#"
            ".###." "#"
            "#..#." "."
            "####." "."
            ".####" "#"
            "#.##." "#"
            "##.##" "."
            "..##." "."
            "#...#" "#"})

(defn pad-state [pad col]
  (str (apply str (repeat pad "."))
       col
       (apply str (repeat pad "."))))

(defn partition-state [pad col]
  (->> (partition 5 1 col)
       (map-indexed (fn [idx item] (vector idx item)))))

(defn plant-or-empty [col plant-number]
  (or (get col plant-number) "."))

(defn get-plants-around [col plant-number]
  (apply str [(plant-or-empty col (- plant-number 2))
              (plant-or-empty col (dec plant-number))
              (plant-or-empty col plant-number)
              (plant-or-empty col (inc plant-number))
              (plant-or-empty col (+ plant-number 2))]))

(defn remove-padded-indexes [col]
  (let [plants (->> (filter (fn [[k v]] (= v "#")) col)
                    (sort-by first))
        min-plant (ffirst plants)
        max-plant (first (last plants))]
    (reduce-kv
      (fn [acc k v] (if (or (< k min-plant) (> k max-plant))
                      acc
                      (assoc acc k v)))
      {}
      col)))

(defn next-generation [col rules]
  (let [min-index (- (first (apply min-key key col)) 5)
        max-index (+ (first (apply max-key key col)) 5)]
    (loop [plant min-index
           acc {}]
      (if (> plant max-index)
        acc
        (let [plants-around (get-plants-around col plant)]
          (recur (inc plant)
                 (assoc acc plant (or (get rules plants-around) "."))))))))

(defn get-amount-for-generation [generation]
  (->> generation
       (filter #(= "#" (second %)))
       (map first)
       (reduce +)))


(defn loop-generations [col rules num-generations]
  (loop [cnt-generation num-generations
         plants (into {} (map-indexed (fn [idx item] (vector idx item)) col))
         amt-for-generation []]
    (when (zero? (mod cnt-generation 10000000)) (println cnt-generation))
    (if (zero? cnt-generation)
      [plants amt-for-generation]
      (let [new-plants (remove-padded-indexes (next-generation plants rules))]
        (recur (dec cnt-generation)
               new-plants
               (conj amt-for-generation (get-amount-for-generation new-plants)))))))

(defn part-1 [col rules]
  (->> (loop-generations col rules 20)
       first
       (filter #(= "#" (second %)))
       (map first)
       (reduce +)))

(defn part-2 [col rules]
  (->> (loop-generations col rules 100)
       second
       (reduce
         (fn [acc item] (conj acc [item (- item (or (ffirst acc) 0))]))
         [])))




