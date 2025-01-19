(ns y24.d15
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [grid :as grid]
   [std :as std]))

(def input
  (let [[left right] (str/split (str/join "\n" (std/read-input 2024 15)) #"\n\n")]
    {:grid (grid/str->Grid left)
     :moves (->> (str/split right #"")
                 (map (fn [c]
                        (case c
                          "<" [-1 0]
                          ">" [1 0]
                          "v" [0 1]
                          "^" [0 -1]
                          nil)))
                 (filter some?))}))

(defn player-pos [grid]
  (->> (for [y (range (grid/height grid))
             x (range (grid/width grid))]
         (when (= "@" (grid/at grid [x y]))
           [x y]))
       (filter some?)
       (first)))

(defn move [grid pos dir]
  (let [next (grid/at grid (std/vec+ pos dir))]
    (case next
      "." [true (-> grid
                    (grid/set-val (std/vec+ pos dir) (grid/at grid pos))
                    (grid/set-val pos "."))]
      "O" (let [[did-move grid'] (move grid (std/vec+ pos dir) dir)]
            (if did-move
              [true (-> grid'
                        (grid/set-val (std/vec+ pos dir) (grid/at grid pos))
                        (grid/set-val pos "."))]
              [false grid]))
      [false grid])))

(defn compress [grid]
  (->> (for [y (range (grid/height grid))
             x (range (grid/width grid))
             :when (= "O" (grid/at grid [x y]))]
         (+ x (* y 100)))
       (apply +)))

(defn pt1 []
  (let [{:keys [grid moves]} input]
    (loop [grid grid
           [dir & moves] moves]
      (if-not dir
        (compress grid)
        (recur (second (move grid (player-pos grid) dir))
               moves)))))

(defn pt2 []
  (let [{:keys [grid moves]} input
        boxes (->> (for [y (range (grid/height grid))
                         x (range (grid/width grid))]
                     (when (= "O" (grid/at grid [x y]))
                       [[(* x 2) y] [(inc (* x 2)) y]]))
                   (filter some?)
                   (set))

        walls (->> (for [y (range (grid/height grid))
                         x (range (grid/width grid))]
                     (when (= "#" (grid/at grid [x y]))
                       [[(* x 2) y] [(inc (* x 2)) y]]))
                   (filter some?)
                   (std/flatten-1)
                   (set))

        start-pos (->> (for [y (range (grid/height grid))
                             x (range (grid/width grid))]
                         (when (= "@" (grid/at grid [x y]))
                           [(* x 2) y]))
                       (filter some?)
                       first)]
    (letfn [(box-at [boxes pos]
              (std/find-first (fn [[a b]] (or (= pos a) (= pos b))) boxes))

            (boxes-to [boxes pos dir]
              (loop [boxes' boxes
                     poss #{pos}
                     found #{}]
                (if (empty? poss)
                  found
                  (let [next (std/vec+ (first poss) dir)
                        box (box-at boxes' next)]
                    (if box
                      (recur (disj boxes' box)
                             (conj (rest poss) (first box) (second box))
                             (conj found box))
                      (recur boxes' (rest poss) found))))))

            (will-bump-wall? [boxes ppos dir]
              (or (contains? walls (std/vec+ ppos dir))
                  (some (fn [[b1 b2]]
                          (or (contains? walls (std/vec+ b1 dir))
                              (contains? walls (std/vec+ b2 dir))))
                        boxes)))

            (move-boxes [all-boxes boxes-to-move dir]
              (set/union (set/difference all-boxes boxes-to-move)
                         (set (map (fn [[b1 b2]]
                                     [(std/vec+ b1 dir) (std/vec+ b2 dir)])
                                   boxes-to-move))))

            (box-dist-to-edge [[[x y] _]]
              (+ (* 100 y) x))]

      (let [[_ end-boxes] (loop [[move & moves'] moves
                                 ppos start-pos
                                 boxes boxes]
                            (if (nil? move)
                              [ppos boxes]
                              (let [boxes-to-push (boxes-to boxes ppos move)]
                                (if (will-bump-wall? boxes-to-push ppos move)
                                  (recur moves' ppos boxes)
                                  (recur moves'
                                         (std/vec+ ppos move)
                                         (move-boxes boxes boxes-to-push move))))))]
        (->> end-boxes
             (map box-dist-to-edge)
             (apply +))))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
