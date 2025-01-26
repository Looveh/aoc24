(ns grid
  (:require
   [clojure.string :as str]))

(def directions
  {:n [0 -1]
   :s [0 1]
   :w [-1 0]
   :e [1 0]})

(def directions'
  {[0 -1] :n
   [0 1] :s
   [-1 0] :w
   [1 0] :e})

(defn direction [[x y] [x' y']]
  (let [dx (- x' x)
        dy (- y' y)]
    (get directions' [dx dy])))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defprotocol IGrid
  (width [this])
  (height [this])
  (rows [this])
  (cols [this])
  (at [this pos])
  (set-val [this pos value])
  (in-bounds? [this pos])
  (pos-of [this v])
  (neighbors [this pos])
  (all-paths
    [this from to]
    [this from to can-step?])
  (walkable-paths [this from can-step?])
  (bfs
    [this from to]
    [this from to can-walk?]
    [this from to can-walk? max-length]
    [this from to can-walk? max-length finish-early?]))

(defrecord Grid [grid]
  IGrid

  (width [_]
    (count (first grid)))

  (height [_]
    (count grid))

  (rows [_]
    grid)

  (cols [_]
    (apply mapv vector grid))

  (in-bounds? [this [x y]]
    (and (<= 0 x (dec (.width this)))
         (<= 0 y (dec (.height this)))))

  (pos-of [this v]
    (first (for [y (range (.height this))
                 x (range (.width this))
                 :when (= v (.at this [x y]))]
             [x y])))

  (at [_ [x y]]
    (get-in grid [y x]))

  (set-val [_ [x y] v]
    (Grid. (assoc-in grid [y x] v)))

  (neighbors [this [x y]]
    (->> directions
         (vals)
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (filter (fn [[x' y']] (.in-bounds? this [x' y'])))))

  (all-paths [this from to]
    (all-paths this from to (constantly true)))

  (all-paths [this from to can-step?]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from])
           paths #{}]
      (if-let [path (peek queue)]
        (let [curr (last path)]
          (if (= curr to)
            (recur (pop queue) (conj paths path))
            (let [next-positions (->> (.neighbors this curr)
                                      (remove (set path))
                                      (filter can-step?))]
              (recur (into (pop queue)
                           (map #(conj path %) next-positions))
                     paths))))
        paths)))

  (walkable-paths [this from can-step?]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from [from]])
           paths {}
           visited #{from}]
      (if-let [[curr path] (peek queue)]
        (let [neighbors (.neighbors this curr)
              next-positions (->> neighbors
                                  (remove visited)
                                  (filter can-step?))
              next-paths (map #(vector % (conj path %)) next-positions)
              new-visited (reduce conj visited (map first next-paths))
              new-paths (reduce (fn [m [pos p]]
                                  (assoc m pos p))
                                paths
                                next-paths)]
          (recur (into (pop queue) next-paths)
                 new-paths
                 new-visited))
        paths)))

  (bfs [this from to]
    (bfs this from to (constantly true)))

  (bfs [this from to can-walk?]
    (bfs this from to can-walk? nil))

  (bfs [this from to can-walk? max-length]
    (bfs this from to can-walk? max-length (constantly nil)))

  (bfs [this from to can-walk? max-length finish-early?]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from])
           visited #{from}]
      (when-let [path (peek queue)]
        (let [curr (last path)]
          (if (= curr to)
            path
            (if (and max-length (>= (count path) max-length))
              (recur (pop queue) visited)
              (let [next-positions
                    (->> (.neighbors this curr)
                         (remove visited)
                         (filter (fn [next-pos]
                                   (let [prev (if (> (count path) 1)
                                                (nth path (- (count path) 2))
                                                nil)
                                         prev-cell (when prev
                                                     {:coord prev
                                                      :val (.at this prev)})
                                         curr-cell {:coord curr
                                                    :val (.at this curr)}
                                         next-cell {:coord next-pos
                                                    :val (.at this next-pos)}]
                                     (can-walk? {:grid this
                                                 :prev prev-cell
                                                 :curr curr-cell
                                                 :next next-cell
                                                 :path path})))))]
                (if-let [early-result (finish-early? curr to)]
                  (into path early-result)
                  (recur (into (pop queue)
                               (map #(conj path %) next-positions))
                         (into visited next-positions)))))))))))

(defn str->Grid [s]
  (->Grid (vec (mapv #(str/split % #"")
                     (str/split-lines s)))))

