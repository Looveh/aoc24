(ns day20
  (:require
   [graph :as graph]
   [grid :as grid]
   [std :refer [slurp-input]]))

(defn parse-input []
  (let [grid (grid/str-> (slurp-input "20.1"))]
    {:grid grid
     :start (grid/pos-of grid "S")
     :end (grid/pos-of grid "E")}))

(defn grid->graph [grid]
  (into {}
        (for [y (range (grid/height grid))
              x (range (grid/width grid))
              :let [pos [x y]]
              :when (not= "#" (grid/at grid pos))]
          [pos (reduce (fn [acc pos]
                         (if (not= "#" (grid/at grid pos))
                           (assoc acc pos 1)
                           acc))
                       {}
                       (grid/neighbors grid pos))])))

(defn find-wall-segments [grid]
  (for [y (range (grid/height grid))
        x (range (grid/width grid))
        :when (= "#" (grid/at grid [x y]))]
    [[x y]]))

(defn time-to-finish [grid start end]
  (first (graph/find-cheapest-path (grid->graph grid) start end)))

(defn cheat [grid wall-segments]
  (reduce (fn [acc pos]
            (grid/set acc pos "."))
          grid
          wall-segments))

(defn pt1 []
  (let [{:keys [grid start end]} (parse-input)
        wall-segments (find-wall-segments grid)
        base-time (time-to-finish grid start end)]
    (println "cheats" (count wall-segments))
    (->> (for [wall-segment wall-segments]
           (let [grid' (cheat grid wall-segment)]
             (time-to-finish grid' start end)))
         (map #(- base-time %))
         (filter #(<= 100 %))
         (count))))

(comment
  (time (pt1))
  ;
  )
