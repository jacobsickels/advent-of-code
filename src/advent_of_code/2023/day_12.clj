(ns advent-of-code.2023.day-12
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2023/day_12.txt")
               (map #(let [[line check] (str/split % #" ")]
                       {:line line :check (map (fn [n] (Integer/parseInt n)) (str/split check #","))}))))

;; I stole this from the link below because I'm dumb and didn't know how memoization works
;; https://github.com/ransoing/AoC23/blob/main/src/day12/solution.ts
(declare consume-dot)
(declare consume-hash)

(defn consume-line [line groups]
  (cond
    ;; No more groups to check and the line still contains #
    (zero? (count groups))
    (if (contains? (set line) \#) 0 1)

    ;; no more characters in line to check
    (zero? (count line))
    0

    ;; line is less than group total plus spaces for \.
    (< (count line) (+ (reduce + groups) (dec (count groups))))
    0

    ;; recur the two branches if we're checking a ?
    (= (first line) \?)
    (+ (consume-dot line groups) (consume-hash line groups))

    ;; skips over the dot to the next characters and keeps group
    (= (first line) \.)
    (consume-dot line groups)

    ;; As long as the next part of the line is the next group
    (= (first line) \#)
    (consume-hash line groups)))

(def memoized-consume-line (memoize consume-line))

(defn consume-dot
  "Skip over dot to rest of string because it isn't part of a group"
  [line groups]
  (memoized-consume-line (subs line 1) groups))

(defn safe-subs
  ([^String s start] (safe-subs s start (count s)))
  ([^String s start end]
   (if (< (count s) start)
     ""
     (subs s start end))))

(defn consume-hash
  "As long as the next set of characters actually defines a group we continue on with the recursion"
  [line groups]
  (if (and (not (str/includes? (safe-subs line 0 (first groups)) "."))
           (not= \# (nth line (first groups) nil)))
    (memoized-consume-line (safe-subs line (inc (first groups))) (rest groups))
    0))

(defn part-1 []
  (->> (map (fn [l] [(:line l) (:check l)]) data)
       (map (fn [[line groups]] (memoized-consume-line line groups)))
       (reduce +)))

(defn expand-line [l]
  (let [line (:line l)
        check (:check l)]
    {:line (str/join "?" (repeat 5 line)) :check (apply concat (repeat 5 check))}))

(defn part-2 []
  (->> (map expand-line data)
       (map (fn [l] [(:line l) (:check l)]))
       (map (fn [[line groups]] (memoized-consume-line line groups)))
       (reduce +)))