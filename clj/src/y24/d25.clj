(ns y24.d25
  (:require [clojure.string :as str]
            [clj.grid :as grid]
            [clj.std :as std]))

(def input
  (std/slurp-input 24 25))

(def input-locks (atom []))
(def input-keys (atom []))

(defn is-key? [grid]
  (= "." (grid/at grid [0 0])))

(doseq [chunk (str/split input #"\n\n")
        :let [grid (grid/str->Grid chunk)
              profile (map (fn [line]
                             (dec (count (filter #(= "#" %) line))))
                           (grid/cols grid))]]
  (if (is-key? grid)
    (swap! input-keys conj profile)
    (swap! input-locks conj profile)))

(defn fits? [lock key]
  (->> (map (fn [l k] (>= 5 (+ l k))) lock key)
       (every? true?)))

(defn pt1 []
  (count (for [l @input-locks
               k @input-keys
               :when (fits? l k)]
           [l k])))

(println :pt1 (time (pt1)))


