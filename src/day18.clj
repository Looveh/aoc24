(ns day18
  (:require
   [clojure.string :as str]
   [graph :as graph]
   [grid :as grid]
   [std :refer [->long read-input]]))

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
  (let [coords (map #(mapv ->long (str/split % #",")) lines)
        empty-grid (vec (repeat height (vec (repeat width "."))))]
    (reduce (fn [grid p]
              (grid/set grid p "#"))
            empty-grid
            coords)))

(defn pt1 []
  (let [[file width height line-cnt] ["18.1" 71 71 1024]
        lines (read-input file)
        grid (create-grid (take line-cnt lines) width height)
        graph (maze->graph grid)]
    (graph/find-cheapest-path graph [0 0] [(dec width) (dec height)])))

(defn pt2 []
  (let [[lines width height] [(read-input "18.1") 71 71]]
    (loop [n 1024]
      (let [grid (create-grid (take n lines) width height)
            graph (maze->graph grid)
            path (graph/find-cheapest-path graph [0 0] [(dec width) (dec height)])]
        (if path
          (recur (inc n))
          (get lines (dec n)))))))

(comment
  (time (pt1))
  (time (pt2))
  ;
  )


