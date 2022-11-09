(ns advent-of-code.2018.day-09-new
  (:require [clojure.set :as set]))

(defn insert-marble [marbles position marble]
  (let [[old-left old-right] (get marbles position)
        [r1 r2] (get marbles old-right)]
    (if (= old-left old-right position)
      [old-right (assoc marbles
                   position [marble marble]
                   marble [position position])]
      [old-right (assoc marbles
                   old-right [marble r2]
                   position [old-left marble]
                   marble [position old-right])])))

(defn get-backwards-7-marble [marbles position]
  (loop [moves 0
         check position]
    (if (= moves 8)
      check
      (recur (inc moves) (first (get marbles check))))))

(defn remove-marble [marbles position]
  (let [[l r] (get marbles position)
        [l1 _] (get marbles l)
        [_ r2] (get marbles r)]
    [(second (get marbles r)) (-> (dissoc marbles position)
                                  (assoc r [l r2]
                                         l [l1 r]))]))

(defn remove-and-score [marbles position]
  (let [marble-to-remove (get-backwards-7-marble marbles position)]
    [marble-to-remove (remove-marble marbles marble-to-remove)]))

(defn play-game [player-count last-marble]
  (loop [marbles {0 [0 0]}
         next-marble 1
         position 0
         scores {}
         player 1]
    (if (= next-marble last-marble)
      (second (apply max-key val scores))
      (if (zero? (mod next-marble 23))
        (let [[marble-to-remove [old-relation new-marbles]] (remove-and-score marbles position)]
          (recur
            new-marbles
            (inc next-marble)
            old-relation
            (update scores player (fn [score] (if (nil? score)
                                                (+ marble-to-remove next-marble)
                                                (+ next-marble score marble-to-remove))))
            (mod (inc player) player-count)))

        (let [[old-relation new-marbles] (insert-marble marbles position next-marble)]
          (recur
            new-marbles
            (inc next-marble)
            old-relation
            scores
            (mod (inc player) player-count)))))))

(defn part-1 []
  (play-game 405 71700))

(defn part-2 []
  (play-game 405 (* 100 71700)))

(defn part-1-test []
  (map
    #(let [highscore (part-1 (:players %) (:score %))]
       (vector highscore (= (:high %) highscore)))
    [{:players 9 :score 25 :high 32}
     {:players 10 :score 1618 :high 8317}
     {:players 13 :score 7999 :high 146373}
     {:players 17 :score 1104 :high 2764}
     {:players 21 :score 6111 :high 54718}
     {:players 30 :score 5807 :high 37305}]))


;; new map, store left and right
;;
;; { 0 [0 0] }
;; { 0 [1 1] 1 [0 0] }
;; { 0 [1 2] 1 [0 2] 2 [0 1] }
