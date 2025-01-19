(ns y24.d11
  (:require [std :as std]))

(def input
  (->> (std/read-input 2024 11)
       (first)
       (std/->cols)))

(def morph
  (memoize
   (fn [stone]
     (cond
       (= "0" stone)
       ["1"]

       (= 0 (mod (count stone) 2))
       (->> (split-at (/ (count stone) 2) stone)
            (map (partial apply str))
            (map std/->long)
            (map str))

       :else
       [(str (* 2024 (std/->long stone)))]))))

(def stone-cnt
  (memoize
   (fn [depth stones]
     (if (>= 0 depth)
       1
       (->> (mapcat morph stones)
            (map #(stone-cnt (dec depth) [%]))
            (map bigint)
            (apply +))))))

(defn pt1 []
  (count (reduce (fn [stones & _]
                   (flatten (map morph stones)))
                 input
                 (range 25))))

(defn pt2 []
  (stone-cnt 75 input))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
