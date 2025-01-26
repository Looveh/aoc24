(ns y24.d03
  (:require [clojure.string :as str]
            [std :as std]))

(def input
  (std/read-input 24 3))

(defn pt1 []
  (let [mul-line (fn [line]
                   (->> line
                        (re-seq #"mul\((\d+),(\d+)\)")
                        (map (fn [[_ a b]]
                               (* (std/->long a) (std/->long b))))
                        (apply +)))]
    (->> input
         (map mul-line)
         (apply +))))

(defn pt2 []
  (->> input
       (apply str)
       (#(str/split % #"do\(\)"))
       (map #(str/split % #"don't\(\).*$"))
       (map first)
       (apply str)
       (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[_ a b]]
              (* (std/->long a) (std/->long b))))
       (apply +)))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
