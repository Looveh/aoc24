(ns grid
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]))

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

(defn vec+ [[x y] [x' y']]
  [(+ x x') (+ y y')])

(defn vec- [[x y] [x' y']]
  [(- x x') (- y y')])

(defn str-> [str]
  (vec (mapv #(str/split % #"") (str/split-lines str))))

(defn ->str [grid]
  (str/join "\n" (map #(str/join "" %) grid)))

(defn width [grid]
  (count (first grid)))

(defn height [grid]
  (count grid))

(defn at [grid [x y]]
  (get-in grid [y x]))

(defn put [grid [x y] value]
  (assoc-in grid [y x] value))

(defn pos-of [grid v]
  (first (for [y (range (height grid))
               x (range (width grid))
               :when (= v (at grid [x y]))]
           [x y])))

(defn in-bounds? [grid [x y]]
  (and (<= 0 x (dec (width grid)))
       (<= 0 y (dec (height grid)))))

(defn neighbors [grid [x y]]
  (->> directions
       (vals)
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       (filter (fn [[x' y']] (in-bounds? grid [x' y'])))))

(defn direction [[x y] [x' y']]
  (let [dx (- x' x)
        dy (- y' y)]
    (get directions' [dx dy])))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn shortest-path-bfs
  {:test
   (fn []
     (let [grid [["." "." "#"]
                 ["." "." "."]
                 ["#" "." "."]]
           exp [[0 0] [0 1] [1 1] [2 1]]
           res (shortest-path-bfs grid [0 0] [2 1])]
       (is (= exp res)))
     (let [grid [["." "#" "."]
                 ["." "#" "."]
                 ["." "." "."]]
           can-walk? #(not= "#" (get-in % [:next :val]))
           exp [[0 0] [0 1] [0 2] [1 2] [2 2] [2 1] [2 0]]
           res (shortest-path-bfs grid [0 0] [2 0] can-walk?)]
       (is (= exp res)))
     (let [grid [["." "#" "."]
                 ["." "#" "."]
                 ["." "." "."]]
           can-walk? #(not= "#" (get-in % [:next :val]))
           res (shortest-path-bfs grid [0 0] [2 0] can-walk? 6)]
       (is (= nil res)))
     (let [grid [["." "#" "."]
                 ["." "#" "."]
                 ["." "." "."]]
           can-walk? #(not= "#" (get-in % [:next :val]))
           exp [[0 0] [0 1] :a :b]
           res (shortest-path-bfs
                grid [0 0] [2 0] can-walk? nil
                (fn [{:keys [curr]}]
                  (when (= [0 1] (:coord curr))
                    [:a :b])))]
       (is (= exp res)))
     (let [grid [["." "#" "."]
                 ["#" "." "#"]
                 ["." "#" "."]]
           can-walk? #(not= "#" (get-in % [:next :val]))
           res (shortest-path-bfs grid [0 0] [2 2] can-walk?)]
       (is (nil? res))))}
  ([grid from to]
   (shortest-path-bfs grid from to (constantly true)))
  ([grid from to can-walk?]
   (shortest-path-bfs grid from to can-walk? nil))
  ([grid from to can-walk? max-length]
   (shortest-path-bfs grid from to can-walk? max-length (constantly nil)))
  ([grid from to can-walk? max-length finish-early?]
   (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from])
          visited #{from}]
     (when-let [path (peek queue)]
       (let [curr (last path)]
         (if (= curr to)
           path
           (if (and max-length (>= (count path) max-length))
             (recur (pop queue) visited)
             (let [next-positions
                   (->> (neighbors grid curr)
                        (remove visited)
                        (filter (fn [next-pos]
                                  (let [prev (if (> (count path) 1)
                                               (nth path (- (count path) 2))
                                               nil)
                                        prev-cell (when prev
                                                    {:coord prev
                                                     :val (at grid prev)})
                                        curr-cell {:coord curr
                                                   :val (at grid curr)}
                                        next-cell {:coord next-pos
                                                   :val (at grid next-pos)}]
                                    (can-walk? {:grid grid
                                                :prev prev-cell
                                                :curr curr-cell
                                                :next next-cell
                                                :path path})))))]
               (if-let [early-result (finish-early? curr to)]
                 (into path early-result)
                 (recur (into (pop queue)
                              (map #(conj path %) next-positions))
                        (into visited next-positions)))))))))))

(defn walkable-paths [grid from can-step?]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from [from]])
         paths {}
         visited #{from}]
    (if-let [[curr path] (peek queue)]
      (let [neighbors (neighbors grid curr)
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

(defn all-paths
  {:test (fn []
           (let [can-step? (fn [grid] #(not= "#" (at grid %)))]
             ;; Test simple 2x2 grid
             (let [grid [["A" "B"]
                         ["C" "D"]]
                   exp #{[[0 0] [0 1] [1 1]]
                         [[0 0] [1 0] [1 1]]}
                   res (all-paths grid [0 0] [1 1])]
               (is (= exp res)))
             (let [grid [["S" "." "."]
                         ["#" "#" "."]
                         ["." "." "E"]]
                   exp #{[[0 0] [1 0] [2 0] [2 1] [2 2]]}
                   res (all-paths grid [0 0] [2 2] (can-step? grid))]
               (is (= exp res)))
             (let [grid [["A" "#"]
                         ["#" "B"]]
                   exp #{}
                   res (all-paths grid [0 0] [1 1] (can-step? grid))]
               (is (= exp res)))))}
  ([grid from to]
   (all-paths grid from to (constantly true)))
  ([grid from to can-step?]
   (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [from])
          paths #{}]
     (if-let [path (peek queue)]
       (let [curr (last path)]
         (if (= curr to)
           (recur (pop queue) (conj paths path))
           (let [next-positions (->> (neighbors grid curr)
                                     (remove (set path))
                                     (filter can-step?))]
             (recur (into (pop queue)
                          (map #(conj path %) next-positions))
                    paths))))
       paths))))
