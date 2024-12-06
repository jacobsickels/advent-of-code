(ns advent-of-code.2024.day-06
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_06.txt")))

(def points (->> (map #(str/split % #"") data)
                 (map-indexed (fn [y line]
                                (map-indexed (fn [x space] [space x y]) line)))
                 (apply concat)))

(def valid-points (set (map (fn [[p x y]] [x y]) points)))

(def rock-points (->> (filter #(= "#" (first %)) points)
                      (map (fn [[p x y]] [x y]))
                      set))

(def guard (first (filter #(contains? #{"v" "^" ">" "<"} (first %)) points)))

(defn move-forward [guard]
  (let [[direction gx gy] guard]
    (cond
      (= "^" direction)
      [gx (dec gy)]

      (= ">" direction)
      [(inc gx) gy]

      (= "v" direction)
      [gx (inc gy)]

      (= "<" direction)
      [(dec gx) gy])))

(defn turn-guard [guard]
  (let [[direction gx gy] guard]
    (cond
      (= "^" direction)
      [">" gx gy]

      (= ">" direction)
      ["v" gx gy]

      (= "v" direction)
      ["<" gx gy]

      (= "<" direction)
      ["^" gx gy])))

(defn next-guard [guard rock-points]
  (let [[direction _ _] guard
        next-guard-point (move-forward guard)]
    (if (contains? rock-points next-guard-point)
      (turn-guard guard)
      (concat [direction] next-guard-point))))

(defn part-1 []
  (loop [g guard
         points #{[(second guard) (nth guard 2)]}]
    (let [[next-direction ngx ngy] (next-guard g rock-points)]
      (if (contains? valid-points [ngx ngy])
        (recur [next-direction ngx ngy] (conj points [ngx ngy]))
        (count points)))))

(def max-height (second (last (sort-by second valid-points))))
(def max-width (first (last (sort-by first valid-points))))
(defn is-point-valid? [[x y]]
  (let [min-height 0
        min-width 0]
    (and (<= min-height y max-height) (<= min-width x max-width))))

(defn does-make-loop? [new-rock-point]
  (loop [g guard
         points #{guard}]
    (let [nxt-guard (next-guard g (conj rock-points new-rock-point))
          [next-direction ngx ngy] nxt-guard]
      (if (contains? points nxt-guard)
        true
        (if (is-point-valid? [ngx ngy])
          (recur [next-direction ngx ngy] (conj points nxt-guard))
          false)))))

(defn part-2 []
  (->> (remove #(contains? rock-points %) valid-points)
       (map does-make-loop?)
       (frequencies)))
