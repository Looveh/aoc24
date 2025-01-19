(ns y24.d01
  (:require [clojure.core.matrix :as cm]
            [clojure.string :as str]
            [std :as std]))

(def input
  (->> (std/read-input 2024 1)
       (map #(str/split % #"\s+"))
       (map #(map parse-long %))
       cm/transpose))

(defn pt1 []
  (let [[left right] (map sort input)]
    (->> (map #(abs (- %1 %2)) left right)
         (apply +))))

(defn pt2 []
  (let [[left right] input]
    (->> (map #(* % (count (filter (partial = %) right))) left)
         (apply +))))

(println :pt1 (time (pt1)))
(println :pt1 (time (pt2)))


