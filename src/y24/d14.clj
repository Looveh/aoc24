(ns y24.d14
  (:require [clojure.string :as str]
            [std :as std]))

(def width 101)
(def height 103)

(def input
  (->> (std/read-input 2024 14)
       (map (fn [line]
              (let [[_ x y dx dy] (re-matches #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)" line)]
                (mapv std/->long [x y dx dy]))))))

(defn move [[x y dx dy] steps]
  [(mod (+ x (* steps dx)) width)
   (mod (+ y (* steps dy)) height)
   dx
   dy])

(defn quadrant [[x y _ _]]
  (let [[w h] [(int (/ width 2)) (int (/ height 2))]]
    (cond
      (and (> x w) (> y h)) 1
      (and (< x w) (> y h)) 2
      (and (< x w) (< y h)) 3
      (and (> x w) (< y h)) 4)))

(defn pt1 []
  (->> input
       (map #(move % 100))
       (group-by quadrant)
       (#(dissoc % nil))
       (vals)
       (map count)
       (apply *)))

(defn adjacents [coords]
  (let [coords' (set coords)]
    (filter (fn [[x y]]
              (some (fn [[dx dy]]
                      (contains? coords' [(+ x dx) (+ y dy)]))
                    [[1 0] [-1 0] [0 1] [0 -1]
                     [1 1] [1 -1] [-1 1] [-1 -1]]))
            coords)))

(defn print-space [robots]
  (->> (for [y (range height)]
         (for [x (range width)]
           (let [n (->> robots
                        (filter (fn [[x' y' _ _]]
                                  (and (= x x') (= y y'))))
                        (count))]
             (if (< 0 n)
               "X"
               "."))))
       (map str/join)
       (str/join "\n")))

(defn pt2 []
  (loop [idx 0
         robots input]
    (when (< idx 50000)
      (if (< 300 (->> robots
                      (map (fn [[x y _ _]] [x y]))
                      (adjacents)
                      (count)))
        (do
          (println idx "\n" (print-space robots))
          idx)
        (recur (inc idx)
               (map #(move % 1) robots))))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
