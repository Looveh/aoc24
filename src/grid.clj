(ns grid
  (:require [clojure.string :as str]))

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

(defn set [grid [x y] value]
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
