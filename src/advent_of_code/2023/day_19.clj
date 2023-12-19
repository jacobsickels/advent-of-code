(ns advent-of-code.2023.day-19
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (read/read-file "resources/2023/day_19.txt"))

(def workflows (->> (partition-by empty? data)
                    first
                    (reduce (fn [acc l]
                              (let [[k v] (str/split l #"\{")
                                    rules-init (apply str (butlast v))
                                    split-rules (str/split rules-init #",")
                                    splitted (map #(let [[arg destination] (str/split % #":")
                                                         [input n] (str/split arg #">|<")]
                                                     (cond
                                                       (str/includes? arg "<")
                                                       [input "<" (Integer/parseInt n) destination]

                                                       (str/includes? arg ">")
                                                       [input ">" (Integer/parseInt n) destination]

                                                       :else [input]))
                                                  split-rules)]
                                (assoc acc k splitted)))
                            {})))

(def parts (->> (partition-by empty? data)
                last
                (map
                  (fn [p]
                    (let [cleaned (apply str (butlast (drop 1 p)))
                          splitted (str/split cleaned #",")
                          final (map #(let [[k v] (str/split % #"=")]
                                        [k (Integer/parseInt v)])
                                     splitted)]
                      (into {} final))))))


(defn find-first [f coll] (first (filter f coll)))

(defn is-accepted? [part]
  (loop [workflow "in"]
    (cond
      (= workflow "R") [false part]
      (= workflow "A") [true part]

      :else (let [work (get workflows workflow)
                  accepted-work (map
                                  #(let [[arg sign n destination] %]
                                     (if (nil? destination)
                                       arg
                                       (cond
                                         (= sign "<")
                                         (if (< (get part arg) n)
                                           destination
                                           nil)

                                         (= sign ">")
                                         (if (> (get part arg) n)
                                           destination
                                           nil)

                                         :else (do (println "CHECK ME") nil))))
                                  work)]
              (recur (first (remove nil? accepted-work)))))))

(defn part-1 []
  (->> (map is-accepted? parts)
       (filter first)
       (map second)
       (map vals)
       (map #(reduce + %))
       (reduce +)))

(defn split-part [part work-piece]
  (let [[arg sign n d] work-piece
        [l r] (get part arg)]
    (cond
      (= sign "<")
      (if (< l n r)
        [(assoc part arg [l (dec n)] "destination" d)
         (assoc part arg [n r] "destination" nil)]
        [part])

      (= sign ">")
      (if (> r n l)
        [(assoc part arg [l n] "destination" nil)
         (assoc part arg [(inc n) r] "destination" d)]
        [part]))))

(defn set-work-defaults [parts work-piece]
  (map (fn [p] (if (nil? (get p "destination"))
                 (assoc p "destination" (first work-piece))
                 p))
       parts))

(defn split-parts-single-work [parts work-piece]
  (if (= 1 (count work-piece))
    (set-work-defaults parts work-piece)
    (reduce
      (fn [acc p]
        (concat acc (split-part p work-piece)))
      [] parts)))

(defn split-parts [parts workflow]
  (loop [flow workflow
         p parts
         results []]
    (if (empty? flow)
      results
      (let [new-parts (split-parts-single-work p (first flow))]
        (recur (rest flow)
               (filter #(nil? (get % "destination")) new-parts)
               (concat results (filter #(not (nil? (get % "destination"))) new-parts)))))))

(defn recursive-split-parts [parts workflow]
  (let [new-parts (split-parts parts workflow)
        destinations (set (map #(get % "destination") new-parts))]
    (cond
      (= #{"A" "R"} destinations)
      new-parts

      :else (mapcat
              (fn [part]
                (let [destination (get part "destination")
                      next-workflow (get workflows destination)]
                  (if (contains? #{"A" "R"} destination)
                    [part]
                    (recursive-split-parts [part] next-workflow))))
              new-parts))))


(defn part-2 []
  (->> (get workflows "in")
       (recursive-split-parts [{"x" [1 4000], "m" [1 4000], "a" [1 4000], "s" [1 4000]}])
       (filter #(= "A" (get % "destination")))
       (map vals)
       (map butlast)
       (map (fn [row] (->> (map #(inc (- (second %) (first %))) row)
                           (reduce *))))
       (reduce +)))
