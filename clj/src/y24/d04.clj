(ns y24.d04
  (:require [std :as std]
            [grid :as grid]))

(def grid
  (std/read-grid 24 4))

(defn pt1 []
  (let [words-at-point (fn [x y]
                         (->> [[[x y]
                                [(+ x 1) y]
                                [(+ x 2) y]
                                [(+ x 3) y]]
                               [[x y]
                                [(- x 1) y]
                                [(- x 2) y]
                                [(- x 3) y]]
                               [[x y]
                                [x (+ y 1)]
                                [x (+ y 2)]
                                [x (+ y 3)]]
                               [[x y]
                                [x (- y 1)]
                                [x (- y 2)]
                                [x (- y 3)]]
                               [[x y]
                                [(+ x 1) (+ y 1)]
                                [(+ x 2) (+ y 2)]
                                [(+ x 3) (+ y 3)]]
                               [[x y]
                                [(+ x 1) (- y 1)]
                                [(+ x 2) (- y 2)]
                                [(+ x 3) (- y 3)]]
                               [[x y]
                                [(- x 1) (+ y 1)]
                                [(- x 2) (+ y 2)]
                                [(- x 3) (+ y 3)]]
                               [[x y]
                                [(- x 1) (- y 1)]
                                [(- x 2) (- y 2)]
                                [(- x 3) (- y 3)]]]
                              (map (fn [coords]
                                     (map #(grid/at grid %) coords)))
                              (map #(apply str %))))]
    (->> (for [x (range (grid/width grid))
               y (range (grid/height grid))
               :let [words (words-at-point x y)]]
           {:x x
            :y y
            :words words
            :hits (filter #(= "XMAS" %) words)})
         (mapcat :hits)
         (count))))

(defn pt2 []
  (let [words-at-point (fn [x y]
                         (->> [[[(- x 1) (- y 1)]
                                [x y]
                                [(+ x 1) (+ y 1)]]
                               [[(+ x 1) (- y 1)]
                                [x y]
                                [(- x 1) (+ y 1)]]]
                              (map (fn [coords]
                                     (map #(grid/at grid %) coords)))
                              (map #(apply str %))))]
    (->> (for [x (range (grid/width grid))
               y (range (grid/height grid))
               :let [words (words-at-point x y)]]
           {:x x
            :y y
            :words words
            :hit? (or (= ["MAS" "MAS"] words)
                      (= ["MAS" "SAM"] words)
                      (= ["SAM" "MAS"] words)
                      (= ["SAM" "SAM"] words))})
         (filter :hit?)
         (count))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
