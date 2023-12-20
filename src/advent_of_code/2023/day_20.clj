(ns advent-of-code.2023.day-20
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.string :as str]))

(def data (read/read-file "resources/2023/day_20.txt"))

(defn get-deps-for-conjunctions [modules]
  (let [conjunction-keys (->> (filter (fn [[k v]] (= :conjunction (:type v))) modules)
                              (map first))]
    (->> (map
           #(vector % (->> (filter (fn [[k v]] (contains? (set (:destinations v)) %)) modules)
                           (map first)))
           conjunction-keys)
         (reduce (fn [acc [k deps]] (assoc-in acc [k :state]
                                              (reduce (fn [acc d] (assoc acc d :low))
                                                      {} deps))) modules))))

(def modules (->> (reduce
                    (fn [acc l] (let [[k v] (str/split l #" -> ")]
                                  (cond
                                    (str/includes? k "%")
                                    (assoc acc (apply str (rest k)) {:type :flip-flop :destinations (str/split v #", ") :state :off})

                                    (str/includes? k "&")
                                    (assoc acc (apply str (rest k)) {:type :conjunction :destinations (str/split v #", ")})

                                    :else (assoc acc k {:type :broadcaster :destinations (str/split v #", ")}))))
                    {} data)
                  get-deps-for-conjunctions))

(defn process-pulse [modules pulse button-iterations]
  (let [origin (:origin pulse)
        pulse-type (:pulse-type pulse)
        pulse-destination (:destination pulse)
        module (get modules pulse-destination)
        destinations (:destinations module)]
    (when (and (= pulse-destination "zh") (= :high pulse-type)) (println pulse button-iterations))
    (cond
      (= pulse-destination "rx")
      [modules [] (= pulse-type :low)]

      (= pulse-destination "output")
      [(assoc modules "output" (conj (get modules "output" []) pulse-type)) [] false]

      (= :conjunction (:type module))
      (let [current-state (:state module)
            next-state (assoc current-state origin pulse-type)
            is-next-all-high? (->> (vals next-state)
                                   (filter #(= % :low))
                                   empty?)]
        [(assoc-in modules [pulse-destination :state] next-state)
         (vec (map #(array-map :origin pulse-destination :pulse-type (if is-next-all-high? :low :high) :destination %) destinations))
         false])


      (= :broadcaster (:type module))
      [modules (map #(array-map :origin pulse-destination :pulse-type pulse-type :destination %) destinations) false]

      (= :flip-flop (:type module))
      (let [current-state (:state module)]
        (if (= :high pulse-type)
          [modules [] false]
          [(assoc-in modules [pulse-destination :state] (if (= :on current-state) :off :on))
           (vec (map #(array-map :origin pulse-destination :pulse-type (if (= :on current-state) :low :high) :destination %) destinations))
           false])))))

(defn do-pulses [initial-pulse input-modules button-iterations]
  (loop [pulses [initial-pulse]
         mods input-modules
         pulse-counts [0 0]]
    (if (empty? pulses)
      [mods pulses pulse-counts false]
      (let [[next-mods next-pulses was-rx-low?] (process-pulse mods (first pulses) button-iterations)]
        (if was-rx-low?
          [mods pulses pulse-counts true]
          (recur (concat (drop 1 pulses) next-pulses)
                 next-mods
                 [(if (= :low (:pulse-type (first pulses))) (inc (first pulse-counts)) (first pulse-counts))
                  (if (= :high (:pulse-type (first pulses))) (inc (second pulse-counts)) (second pulse-counts))]))))))


(defn part-1 []
  (let [button-pulse {:origin "button" :pulse-type :low :destination "broadcaster"}]
    (loop [mods modules
           pulse-counts [0 0]
           iterations 0]
      (if (= iterations 1000)
        (reduce * pulse-counts)
        (let [[next-mods _ [low-count high-count]] (do-pulses button-pulse mods iterations)]
          (recur next-mods
                 [(+ (first pulse-counts) low-count) (+ (second pulse-counts) high-count)]
                 (inc iterations)))))))

(defn part-2 []
  (let [button-pulse {:origin "button" :pulse-type :low :destination "broadcaster"}]
    (loop [mods modules
           pulse-counts [0 0]
           iterations 0
           was-rx-low? false]
      (if was-rx-low?
        iterations
        (let [[next-mods _ [low-count high-count] was-rx-low?] (do-pulses button-pulse mods iterations)
              zh-has-high? (->> (:state (get next-mods "zh"))
                                vals
                                (filter #(= % :high))
                                empty?
                                not)]

          (when zh-has-high? (println iterations))
          (recur next-mods
                 [(+ (first pulse-counts) low-count) (+ (second pulse-counts) high-count)]
                 (inc iterations)
                 was-rx-low?))))))

; logging in process-pulse  (when (and (= pulse-destination "zh") (= :high pulse-type)) (println pulse button-iterations))
; "zh" was the :conjunction that would send a low pulse to "rx" when it has all highs
;{:origin ns, :pulse-type :high, :destination zh} 113009
;{:origin dl, :pulse-type :high, :destination zh} 113369
;{:origin vd, :pulse-type :high, :destination zh} 116429
;{:origin bh, :pulse-type :high, :destination zh} 116590
;{:origin ns, :pulse-type :high, :destination zh} 116776
;{:origin dl, :pulse-type :high, :destination zh} 117148
;{:origin vd, :pulse-type :high, :destination zh} 120310
;{:origin bh, :pulse-type :high, :destination zh} 120351
;{:origin ns, :pulse-type :high, :destination zh} 120543
;{:origin dl, :pulse-type :high, :destination zh} 120927
;{:origin bh, :pulse-type :high, :destination zh} 124112
;{:origin vd, :pulse-type :high, :destination zh} 124191

;; Finding Cycles
;{:origin ns, :pulse-type :high, :destination zh} 113009
;{:origin ns, :pulse-type :high, :destination zh} 116776
;{:origin ns, :pulse-type :high, :destination zh} 120543 (3767)

;{:origin dl, :pulse-type :high, :destination zh} 113369
;{:origin dl, :pulse-type :high, :destination zh} 117148
;{:origin dl, :pulse-type :high, :destination zh} 120927 (3779)

;{:origin vd, :pulse-type :high, :destination zh} 116429
;{:origin vd, :pulse-type :high, :destination zh} 120310
;{:origin vd, :pulse-type :high, :destination zh} 124191 (3881)

;{:origin bh, :pulse-type :high, :destination zh} 116590
;{:origin bh, :pulse-type :high, :destination zh} 120351
;{:origin bh, :pulse-type :high, :destination zh} 124112 (3761)

(defn part-2-lcm []
  (apply utils/lcmv [3767 3779 3881 3761]))