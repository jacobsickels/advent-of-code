(ns advent-of-code.2025.day-02
  (:require [clojure.string :as str]))

(def data-2 (->> (str/split "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" #",")
                 (map (fn [s] (str/split s #"-")))))


(def real-data-2 (->> (str/split "7777742220-7777814718,3201990-3447830,49-86,653243-683065,91-129,24-41,1-15,2678-4638,1407-2511,221-504,867867-942148,1167452509-1167622686,9957459726-9957683116,379068-535983,757-1242,955118-1088945,297342-362801,548256-566461,4926-10075,736811-799457,1093342-1130060,620410-651225,65610339-65732429,992946118-993033511,5848473-5907215,17190619-17315301,203488-286290,15631-36109,5858509282-5858695889,87824047-87984031,1313113913-1313147594,795745221-795825571,46303-100636,4743038-4844422" #",")
                      (map (fn [s] (str/split s #"-")))))

(defn is-valid-id? [check-id]
  (let [parts (partition-all 2 check-id)
        [p1 p2] parts]
    (= p1 p2)))

(defn get-left-half [n]
  (let [s (str n)                      ; Convert number to a string
        midpoint (if (odd? (count s)) (quot (inc (count s)) 2) (quot (count s) 2))   ; Find the middle index using integer division
        left-half-str (subs s 0 midpoint)] ; Get the substring from the start to the midpoint
    (read-string left-half-str)))


(defn get-left-halves [[p1 p2]]
  (let [count-n1 (count p1)
        count-n2 (count p2)]
    (cond
      (and (even? count-n1) (even? count-n2))
      (let [midpoint-n1 (quot count-n1 2)
            midpoint-n2 (quot count-n2 2)]
        [(read-string (subs p1 0 midpoint-n1))
         (read-string (subs p2 0 midpoint-n2))])

      (and (even? count-n1) (odd? count-n2))
      (let [midpoint-n1 (quot count-n1 2)
            midpoint-n2 (inc (quot count-n2 2))]
        [(read-string (subs p1 0 midpoint-n1))
         (read-string (subs p2 0 midpoint-n2))])

      (and (odd? count-n1) (even? count-n2))
      (let [midpoint-n1 (quot count-n1 2)
            midpoint-n1 (if (zero? midpoint-n1) 1 midpoint-n1)
            midpoint-n2 (quot count-n2 2)
            midpoint-n2 (if (zero? midpoint-n2) 1 midpoint-n2)]
        (println p1 p2 midpoint-n1 midpoint-n2)
        [(read-string (subs p1 0 midpoint-n1))
         (read-string (subs p2 0 midpoint-n2))])

      (and (odd? count-n1) (odd? count-n2))
      (let [midpoint-n1 (inc (quot count-n1 2))
            midpoint-n2 (inc (quot count-n2 2))]
        [(read-string (subs p1 0 midpoint-n1))
         (read-string (subs p2 0 midpoint-n2))]))))


(defn get-numbers-between [[p1 p2]]
  (let [n1 (BigInteger. p1)
        n2 (BigInteger. p2)
        [n1-left n2-left] (get-left-halves [p1 p2])]
    (->> (map #(BigInteger. (str % %)) (range (dec n1-left) (inc n2-left)))
         (filter #(<= n1 % n2)))))

(defn two-part-1 []
  (->> (map get-numbers-between real-data-2)
       (flatten)
       (reduce +)))

(defn is-invalid-partition? [parts]
  (and (apply = parts)
       (-> (map count parts)
           (frequencies)
           (count)
           (= 1))))

(defn get-invalid-id-partitions [id]
  (loop [cnt (count id)
         acc []]
    (if (zero? cnt)
      (->> (map #(map (fn [c] (apply str c)) %) acc)
           (filter #(<= 2 (count %)))
           (filter is-invalid-partition?))
      (recur (dec cnt) (conj acc (partition-all cnt id))))))

(defn is-invalid-id-part-2? [id]
  (let [parts (get-invalid-id-partitions id)]
    (not (empty? parts))))

(defn get-invalid-in-rage [[p1 p2]]
  (let [n1 (BigInteger. p1)
        n2 (BigInteger. p2)]
    (filter #(is-invalid-id-part-2? (str %))
            (range n1 (inc n2)))))

(defn two-part-2 []
  (->> (map get-invalid-in-rage real-data-2)
       (flatten)
       (reduce +)))
