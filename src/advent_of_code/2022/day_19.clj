(ns advent-of-code.2022.day-19)

(def test-blueprint {:blueprint      1
                     :ore-robot      {:ore 4}
                     :clay-robot     {:ore 2}
                     :obsidian-robot {:ore 3 :clay 14}
                     :geode-robot    {:ore 2 :obsidian 7}})

(defn obsidian-robot-needs [blueprint production resources]
  (let [robot (:obsidian-robot blueprint)
        ore (+ (:ore-robots production) (:ore resources))
        clay (+ (:clay-robots production) (:clay resources))]
    (cond
      (< (Math/ceilDiv clay ore) (Math/floorDiv (:clay robot) (:ore robot)))
      :clay-robot

      :else nil)))

(defn geode-robot-needs [blueprint production resources]
  (let [robot (:geode-robot blueprint)
        ore (+ (:ore-robots production) (:ore resources))
        obsidian (+ (:obsidian-robots production) (:obsidian resources))]
    (cond
      (< (Math/ceilDiv obsidian ore) (Math/floorDiv (:obsidian robot) (:ore robot)))
      :obsidian-robot

      :else nil)))

(defn can-buy-clay-robot? [blueprint resources]
  (>= (:ore resources) (get-in blueprint [:clay-robot :ore])))

(defn can-buy-obsidian-robot? [blueprint resources]
  (and (>= (:ore resources) (get-in blueprint [:obsidian-robot :ore]))
       (>= (:clay resources) (get-in blueprint [:obsidian-robot :clay]))))

(defn can-buy-geode-robot? [blueprint resources]
  (and (>= (:ore resources) (get-in blueprint [:geode-robot :ore]))
       (>= (:obsidian resources) (get-in blueprint [:geode-robot :obsidian]))))

(defn buy-clay-robot [blueprint robots resources]
  (let [ore-cost (get-in blueprint [:clay-robot :ore])]
    (list (update robots :clay-robots inc) (update resources :ore (fn [n] (- n ore-cost))))))

(defn buy-obsidian-robot [blueprint robots resources]
  (let [ore-cost (get-in blueprint [:obsidian-robot :ore])
        clay-cost (get-in blueprint [:obsidian-robot :clay])]
    (list (update robots :obsidian-robots inc)
          (-> (update resources :ore (fn [n] (- n ore-cost)))
              (update :clay (fn [n] (- n clay-cost)))))))

(defn buy-geode-robot [blueprint robots resources]
  (let [ore-cost (get-in blueprint [:geode-robot :ore])
        obsidian-cost (get-in blueprint [:geode-robot :obsidian])]
    (list (update robots :geode-robots inc)
          (-> (update resources :ore (fn [n] (- n ore-cost)))
              (update :obsidian (fn [n] (- n obsidian-cost)))))))

(defn robots-get-resources [robots resources]
  (assoc resources :ore (+ (:ore resources) (:ore-robots robots))
                   :clay (+ (:clay resources) (:clay-robots robots))
                   :obsidian (+ (:obsidian resources) (:obsidian-robots robots))
                   :geode (+ (:geode resources) (:geode-robots robots))))

(defn do-blueprint [blueprint]
  (loop [robots {:ore-robots 1 :clay-robots 0 :obsidian-robots 0 :geode-robots 0}
         resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
         minutes 24]

    (if (zero? minutes)
      [robots resources]
      (let [obsidian-robot-need (obsidian-robot-needs blueprint robots resources)
            geode-robot-need (geode-robot-needs blueprint robots resources)]
        (println (- 24 minutes) robots resources)
        (cond
          (and (nil? geode-robot-need) (can-buy-geode-robot? blueprint resources))
          (let [[new-robots new-resources] (buy-geode-robot blueprint robots resources)]
            (recur new-robots
                   (robots-get-resources robots new-resources)
                   (dec minutes)))

          (and (= geode-robot-need :obsidian-robot) (can-buy-obsidian-robot? blueprint resources))
          (let [[new-robots new-resources] (buy-obsidian-robot blueprint robots resources)]
            (recur new-robots
                   (robots-get-resources robots new-resources)
                   (dec minutes)))

          (and (= obsidian-robot-need :clay-robot) (can-buy-clay-robot? blueprint resources))
          (let [[new-robots new-resources] (buy-clay-robot blueprint robots resources)]
            (recur new-robots
                   (robots-get-resources robots new-resources)
                   (dec minutes)))

          :else
          (recur robots
                 (robots-get-resources robots resources)
                 (dec minutes)))))))


