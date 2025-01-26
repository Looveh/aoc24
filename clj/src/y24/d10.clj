(ns y24.d10
  (:require [clj.std :as std]))

(def input
  (->> (std/read-input 24 10)
       (std/->grid)
       (mapv (partial mapv #(when (not= "." %) (std/->long %))))))

(defn pt1 []
  (let [graph (->> input
                   (std/grid->graph (fn [a b]
                                      (and a b (= a (dec b))))))]
    (->> (for [zero (->> (vals graph)
                         (filter #(= 0 (:val %)))
                         (map :coord))
               nine (->> (vals graph)
                         (filter #(= 9 (:val %)))
                         (map :coord))]
           (std/graph-paths graph zero nine))
         (filter seq)
         (count))))

(defn pt2 []
  (let [graph (->> input
                   (std/grid->graph (fn [a b]
                                      (and a b (= a (dec b))))))]
    (->> (for [zero (->> (vals graph)
                         (filter #(= 0 (:val %)))
                         (map :coord))
               nine (->> (vals graph)
                         (filter #(= 9 (:val %)))
                         (map :coord))]
           (std/graph-paths graph zero nine))
         (filter seq)
         (map count)
         (apply +))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
