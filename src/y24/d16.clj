(ns y24.d16
  (:require [std :as std]
            [grid :as grid]))

(defn maze->graph [grid]
  (let [width (grid/width grid)
        height (grid/height grid)

        start-pos
        (->> (for [y (range height)
                   x (range width)
                   :when (= "S" (grid/at grid [x y]))]
               [x y])
             first)

        end-pos
        (->> (for [y (range height)
                   x (range width)
                   :when (= "E" (grid/at grid [x y]))]
               [x y])
             first)

        walkable?
        (fn [pos]
          (and (grid/in-bounds? grid pos)
               (not= "#" (grid/at grid pos))))

        rotation-cost
        (fn [from-dir to-dir]
          (cond
            (= from-dir to-dir) 0
            (or (and (#{:n :s} from-dir) (#{:e :w} to-dir))
                (and (#{:e :w} from-dir) (#{:n :s} to-dir))) 1000
            :else 2000))

        build-graph
        (fn []
          (let [coords (for [y (range height)
                             x (range width)
                             :when (not= "#" (grid/at grid [x y]))]
                         [x y])
                nodes (mapcat (fn [[x y]]
                                [[[x y] :n]
                                 [[x y] :s]
                                 [[x y] :e]
                                 [[x y] :w]])
                              coords)]
            (reduce
             (fn [graph [from from-dir :as node]]
               (let [edges (->> (grid/neighbors grid from)
                                (filter walkable?)
                                (map (fn [to]
                                       (let [to-dir (grid/direction from to)]
                                         {[to to-dir] (+ 1 (rotation-cost from-dir to-dir))})))
                                (apply merge))]
                 (assoc graph node edges)))
             {}
             nodes)))]

    [(build-graph) start-pos end-pos]))

(def input
  (let [grid (std/read-grid 2024 16)]
    (maze->graph grid)))

(defn pt1 []
  (let [[graph start-pos end-pos] input
        a (first (std/graph-cheapest-path graph [start-pos :e] [end-pos :n]))
        b (first (std/graph-cheapest-path graph [start-pos :e] [end-pos :s]))
        c (first (std/graph-cheapest-path graph [start-pos :e] [end-pos :w]))
        d (first (std/graph-cheapest-path graph [start-pos :e] [end-pos :e]))]
    (->> [a b c d]
         (filter some?)
         (sort-by first)
         (first))))

(defn pt2 []
  (let [[graph start-pos end-pos] input
        a (std/graph-cheapest-path graph [start-pos :e] [end-pos :n])
        b (std/graph-cheapest-path graph [start-pos :e] [end-pos :s])
        c (std/graph-cheapest-path graph [start-pos :e] [end-pos :w])
        d (std/graph-cheapest-path graph [start-pos :e] [end-pos :e])]
    (->> [a b c d]
         (filter some?)
         (sort-by ffirst)
         (first)
         (mapcat second)
         (map first)
         (set)
         (count))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
