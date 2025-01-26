(ns y24.d18
  (:require [clojure.string :as str]
            [clj.std :as std]
            [clj.grid :as grid]))

(defn maze->graph [grid]
  (let [walkable? (fn [p]
                    (and (grid/in-bounds? grid p)
                         (= "." (grid/at grid p))))
        coords (for [y (range (grid/height grid))
                     x (range (grid/width grid))
                     :let [p [x y]]
                     :when (walkable? p)]
                 p)]
    (reduce
     (fn [graph from]
       (let [edges (->> (grid/neighbors grid from)
                        (filter walkable?)
                        (map (fn [to] {to 1}))
                        (apply merge))]
         (assoc graph from edges)))
     {}
     coords)))

(defn create-grid [lines width height]
  (let [coords (map #(mapv std/->long (str/split % #",")) lines)
        empty-grid (grid/->Grid (vec (repeat height (vec (repeat width ".")))))]
    (reduce (fn [grid p]
              (grid/set-val grid p "#"))
            empty-grid
            coords)))

(defn pt1 []
  (let [[width height line-cnt] [71 71 1024]
        lines (std/read-input 24 18)
        grid (create-grid (take line-cnt lines) width height)
        graph (maze->graph grid)]
    (first (std/graph-cheapest-path graph [0 0] [(dec width) (dec height)]))))

(defn pt2 []
  (let [[lines width height] [(std/read-input 24 18) 71 71]]
    (loop [n 1024]
      (let [grid (create-grid (take n lines) width height)
            graph (maze->graph grid)
            path (std/graph-cheapest-path graph [0 0] [(dec width) (dec height)])]
        (if path
          (recur (inc n))
          (get lines (dec n)))))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))



