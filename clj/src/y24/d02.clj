(ns y24.d02
  (:require [clojure.string :as str]
            [clj.std :as std]))

(def input
  (->> (std/read-input 24 2)
       (map #(str/split % #"\s+"))
       (map (partial map std/->long))))

(defn pt1 []
  (let [safe? (fn [line]
                (and (or (= line (sort line))
                         (= (reverse line) (sort line)))
                     (->> (partition 2 1 line)
                          (every? (fn [[a b]]
                                    (<= 1 (abs (- a b)) 3))))))]
    (->> input
         (filter safe?)
         (count))))

(defn pt2 []
  (let [safe? (fn [line]
                (and (or (= line (sort line))
                         (= (reverse line) (sort line)))
                     (->> (partition 2 1 line)
                          (every? (fn [[a b]]
                                    (<= 1 (abs (- a b)) 3))))))
        safe'? (fn [line]
                 (let [permutations (for [i (range (count line))]
                                      (std/remove-nth i line))]
                   (some safe? permutations)))]

    (->> input
         (filter safe'?)
         (count))))

(println :pt1 (time (pt1)))
(println :pt1 (time (pt2)))
