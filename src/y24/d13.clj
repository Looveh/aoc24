(ns y24.d13
  (:require [clojure.core.logic :as cl]
            [clojure.core.logic.fd :as clfd]
            [std :as std]))

(def input
  (for [[l1 l2 l3 _] (partition 4 (std/read-input 2024 13))]
    (let [[_ ax ay] (re-matches #"Button A: X\+(\d+), Y\+(\d+)" l1)
          [_ bx by] (re-matches #"Button B: X\+(\d+), Y\+(\d+)" l2)
          [_ tx ty] (re-matches #"Prize: X=(\d+), Y=(\d+)" l3)]
      (mapv std/->long [ax ay bx by tx ty]))))

(defn solve [[ax ay bx by tx ty] offset]

  ; I didn't come here to do maths on paper

  (cl/run* [q]
    (cl/fresh [an bn]
      (clfd/in an (clfd/interval 0 (+ (* 2 offset) 100)))
      (clfd/in bn (clfd/interval 0 (+ (* 2 offset) 100)))
      (clfd/eq (= (+ tx offset) (+ (* an ax) (* bn bx)))
               (= (+ ty offset) (+ (* an ay) (* bn by))))
      (cl/== q [an bn]))))

(defn cheapest-solution [vars offset]
  (->> (solve vars offset)
       (map (fn [[a b]] (+ (* a 3) b)))
       (sort)
       (first)))

(defn pt1 []
  (->> input
       (map #(cheapest-solution % 0))
       (filter some?)
       (apply +)))

(defn pt2 []
  (->> input
       (map #(cheapest-solution % 10000000000000))
       (filter some?)
       (apply +)))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
