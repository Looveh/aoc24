(ns y24.d08
  (:require [clj.std :as std]
            [clj.grid :as grid]))

(def grid
  (std/read-grid 24 8))

(defn pt1 []
  (letfn [(aligned-antenna? [[x y] [dx dy]]
            (let [a1 (grid/at grid [(+ x dx) (+ y dy)])
                  a2 (grid/at grid [(+ x dx dx) (+ y dy dy)])]
              (and a1 a2 (not= a1 ".") (= a1 a2) (not (= dx dy 0)))))

          (antinode? [p]
            (let [offsets (for [dx (range (- (grid/width grid))
                                          (grid/width grid))
                                dy (range (- (grid/height grid))
                                          (grid/height grid))]
                            [dx dy])]
              (->> offsets
                   (std/find-first #(aligned-antenna? p %))
                   (boolean))))]

    (->> (for [x (range (grid/width grid))
               y (range (grid/height grid))]
           [x y])
         (filter antinode?)
         (count))))

(defn pt2 []
  (letfn [(aligned-antenna? [[x y] [dx dy]]
            (cond
              (and (= dx dy 0)
                   (not= "." (grid/at grid [x y])))
              true

              (= dx dy 0)
              false

              :else
              (let [as (loop [acc []
                              i 1]
                         (let [a (grid/at grid [(+ x (* i dx))
                                                (+ y (* i dy))])]
                           (if a
                             (recur (conj acc a) (inc i))
                             acc)))]
                (->> as
                     (filter #(not= "." %))
                     (group-by identity)
                     (vals)
                     (filter #(<= 2 (count %)))
                     (seq)
                     (boolean)))))

          (antinode? [p]
            (let [offsets (for [dx (range (- (grid/width grid))
                                          (grid/width grid))
                                dy (range (- (grid/height grid))
                                          (grid/height grid))]
                            [dx dy])]
              (->> offsets
                   (std/find-first #(aligned-antenna? p %))
                   (boolean))))]

    (->> (for [x (range (grid/width grid))
               y (range (grid/height grid))]
           [x y])
         (filter antinode?)
         (count))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
