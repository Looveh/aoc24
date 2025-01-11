(ns day20
  (:require [std :refer [slurp-input]]
            [grid :as grid]))

(def grid (grid/str-> (slurp-input "20.1")))
(def start (grid/pos-of grid "S"))
(def end (grid/pos-of grid "E"))

(println :start start)
(println :end end)

(def cache
  (atom {[start start] 0
         [end end] 0}))

(defn steppable? [coord]
  (not (= "#" (grid/at grid coord))))

(def start->end
  (grid/shortest-path-bfs grid start end
                          (fn [{:keys [curr]}]
                            (steppable? (:coord curr)))))

(doseq [[to path] (grid/walkable-paths grid start steppable?)]
  (swap! cache assoc [start to] (dec (count path))))

(doseq [[from path] (grid/walkable-paths grid end steppable?)]
  (swap! cache assoc [from end] (dec (count path))))

(def base (get @cache [start end]))

(println :base base)

(def target-saving 100)

(def max-length (- base target-saving))

(defn coords-around [pos dist]
  (for [dx (range (- dist) (inc dist))
        dy (range (- dist) (inc dist))
        :let [coord (grid/vec+ pos [dx dy])]
        :when (and (not= pos coord)
                   (grid/in-bounds? grid coord)
                   (steppable? coord)
                   (<= (grid/manhattan-distance pos coord) dist))]
    coord))

(defn find-cheat-cnt [cheat-length]
  (let [cheat-cnt (atom 0)]
    (doseq [i (range (count start->end))
            :let [from (nth start->end i)]]
      (doseq [to (coords-around from cheat-length)
              :let [dist (+ i
                            (grid/manhattan-distance from to)
                            (get @cache [to end]))]
              :when (<= dist max-length)]
        (swap! cheat-cnt inc)))
    @cheat-cnt))

(println :pt1 (find-cheat-cnt 2))
(println :pt2 (find-cheat-cnt 20))

