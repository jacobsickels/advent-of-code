(ns advent-of-code.shared.read-file
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io PushbackReader)))

(defn read-file [file]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce conj [] (line-seq reader))))

(defn load-edn
  [resource-file]
  (edn/read (PushbackReader. (io/reader (io/resource resource-file)))))

(defn integers
  [s & {:keys [negative?]
        :or {negative? true}}]
  (mapv parse-long
        (re-seq (if negative? #"-?\d+" #"\d+") s)))

(defn string->digits [s]
  (->> (str/split s #"")
       (map parse-long)
       (filterv some?)))

(defn parse-multiline-string
  [col & [parse-fn {:keys [word-sep nl-sep]}]]
  (->> col
       ((if (nil? parse-fn) identity
                            (partial mapv
                                     (case parse-fn)
                                     :int parse-long
                                     :ints integers
                                     :digits string->digits
                                     :chars vec
                                     :words #(str/split % (or word-sep #" "))
                                     parse-fn)))))

(defn read-input
  [input & [parse-fn seps]]
  (let [name (if (int? input)
               (format "%02d" input)
               input)]
    (-> (str "resources/" name ".txt")
        (read-file)
        (parse-multiline-string parse-fn seps))))

(defn read-input-paragraphs
  [input & [parse-fn word-sep]]
  (->> (read-input input nil {:nl-sep #"\n\n"})
       (mapv #(parse-multiline-string % parse-fn {:word-sep word-sep}))))