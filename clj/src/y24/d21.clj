(ns y24.d21
  (:require [clj.std :as std]
            [clj.grid :as grid]))

(def input
  (std/read-input 24 21))

(def numpad
  (grid/->Grid
   [["7" "8" "9"]
    ["4" "5" "6"]
    ["1" "2" "3"]
    [nil "0" "A"]]))

(def keypad
  (grid/->Grid
   [[nil "^" "A"]
    ["<" "v" ">"]]))

(def directions
  {[0 -1] "^"
   [0 1] "v"
   [-1 0] "<"
   [1 0] ">"})

(defn coords->dirs [path]
  (for [i (range (dec (count path)))
        :let [from (nth path i)
              to (nth path (inc i))]]
    (directions (std/vec- to from))))

(def paths-between
  (memoize
   (fn [pad from to]
     (->> (grid/all-paths pad
                          (grid/pos-of pad from)
                          (grid/pos-of pad to)
                          #(not (nil? (grid/at pad %))))
          (map coords->dirs)
          (map #(apply str %))
          (map #(str % "A"))))))

(def cost
  (memoize
   (fn [sub-target level top-level?]
     (if (= 0 level)
       (count sub-target)
       (let [s (str "A" sub-target)]
         (->> (for [i (range (dec (count s)))
                    :let [from (str (nth s i))
                          to (str (nth s (inc i)))]]
                (paths-between (if top-level? numpad keypad) from to))
              (map (fn [paths]
                     (apply min (map #(cost % (dec level) false) paths))))
              (apply +)))))))

(defn multiplier [target]
  (std/->long (second (re-matches #"(\d+)A" target))))

(defn complexity [target n]
  (* (multiplier target)
     (cost target n true)))

(defn solve [input n]
  (->> input
       (map #(complexity % n))
       (apply +)))

(println :pt1 (solve input 3))
(println :pt2 (solve input 26))



