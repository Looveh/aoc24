(ns y24.d06
  (:require [clj.std :as std]))

(def grid
  (std/read-grid 24 6))

(defn pt1 []
  (let [start-dir [0 -1]
        start-pos (->> (for [x (range (.width grid))
                             y (range (.height grid))]
                         (when (= "^" (.at grid [x y]))
                           [x y]))
                       (std/find-first some?))

        rotate (fn [dir]
                 (case dir
                   [0 -1] [1 0]
                   [1 0] [0 1]
                   [0 1] [-1 0]
                   [-1 0] [0 -1]))

        move (fn [[px py] [dx dy]]
               [(+ px dx) (+ py dy)])

        in-bounds? (fn [[px py]]
                     (and (<= 0 px (dec (.width grid)))
                          (<= 0 py (dec (.height grid)))))

        blocked? (fn [p d]
                   (let [[dx dy] (move p d)]
                     (= "#" (.at grid [dx dy]))))

        visited (loop [p start-pos
                       d start-dir
                       visited #{p}]
                  (cond
                    (not (in-bounds? p))
                    visited

                    (blocked? p d)
                    (recur (move p (rotate d))
                           (rotate d)
                           (conj visited p))

                    :else
                    (recur (move p d)
                           d
                           (conj visited p))))]

    (count visited)))

(defn pt2 []
  (let [start-dir [0 -1]
        start-pos (->> (for [x (range (.width grid))
                             y (range (.height grid))]
                         (when (= "^" (.at grid [x y]))
                           [x y]))
                       (std/find-first some?))

        rotate (fn [dir]
                 (case dir
                   [0 -1] [1 0]
                   [1 0] [0 1]
                   [0 1] [-1 0]
                   [-1 0] [0 -1]))

        move (fn [[px py] [dx dy]]
               [(+ px dx) (+ py dy)])

        in-bounds? (fn [g [px py]]
                     (and (<= 0 px (dec (.width g)))
                          (<= 0 py (dec (.height g)))))

        blocked? (fn [g p d]
                   (let [[dx dy] (move p d)]
                     (= "#" (.at g [dx dy]))))

        place-obstacle (fn [g [x y]]
                         (when-not (contains? #{"^" "#"} (.at g [x y]))
                           (.set-val g [x y] "#")))

        loops? (fn [g]
                 (loop [p start-pos
                        d start-dir
                        visited #{}]
                   (cond
                     (contains? visited [p d])
                     true

                     (not (in-bounds? g p))
                     false

                     (blocked? g p d)
                     (recur p (rotate d) visited)

                     :else
                     (recur (move p d) d (conj visited [p d])))))]

    (->> (for [x (range (.width grid))
               y (range (.height grid))]
           (when-let [grid' (place-obstacle grid [x y])]
             (loops? grid')))
         (filter true?)
         (count))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
