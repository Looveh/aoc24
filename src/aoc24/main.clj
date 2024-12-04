(ns aoc24.main
  (:require [clojure.core.matrix :as cm]
            [clojure.set :as set]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Helpers

(defn read-input [filename]
  (-> (str "inputs/" filename ".txt")
      (slurp)
      (str/split-lines)))

(defn ->cols [line]
  (str/split line #"\s+"))

(defn ->grid [lines]
  (vec (mapv #(str/split % #"") lines)))

(defn ->int [s]
  (Integer/parseInt s))

;; ---------------------------------------------------------------------------
;; Day 1

(defn day-1-1 []
  (let [[left right] (->> (read-input "1.1")
                          (map ->cols)
                          (map #(map ->int %))
                          (cm/transpose)
                          (map sort))]
    (->> (map #(abs (- %1 %2)) left right)
         (apply +))))

(defn day-1-2 []
  (let [[left right] (->> (read-input "1.1")
                          (map ->cols)
                          (map #(map ->int %))
                          (cm/transpose))]
    (->> (map #(* % (count (filter (partial = %) right))) left)
         (apply +))))

(comment
  (day-1-1)
  (day-1-2)

  ;
  )

;; ---------------------------------------------------------------------------
;; Day 2

(defn day-2-1 []
  ; TODO
  )

(defn day-2-2 []
  ; TODO
  )

;; ---------------------------------------------------------------------------
;; Day 3

(defn day-3-1 []
  ; TODO
  )

(defn day-3-2 []
  ; TODO
  )

;; ---------------------------------------------------------------------------
;; Day 4

(defn day-4-1 []
  (let [grid (->> (read-input "4.1")
                  (->grid))

        words-at-point (fn [x y]
                         (->> [[[y x]
                                [(+ y 1) x]
                                [(+ y 2) x]
                                [(+ y 3) x]]
                               [[y x]
                                [(- y 1) x]
                                [(- y 2) x]
                                [(- y 3) x]]
                               [[y x]
                                [y (+ x 1)]
                                [y (+ x 2)]
                                [y (+ x 3)]]
                               [[y x]
                                [y (- x 1)]
                                [y (- x 2)]
                                [y (- x 3)]]
                               [[y x]
                                [(+ y 1) (+ x 1)]
                                [(+ y 2) (+ x 2)]
                                [(+ y 3) (+ x 3)]]
                               [[y x]
                                [(+ y 1) (- x 1)]
                                [(+ y 2) (- x 2)]
                                [(+ y 3) (- x 3)]]
                               [[y x]
                                [(- y 1) (+ x 1)]
                                [(- y 2) (+ x 2)]
                                [(- y 3) (+ x 3)]]
                               [[y x]
                                [(- y 1) (- x 1)]
                                [(- y 2) (- x 2)]
                                [(- y 3) (- x 3)]]]
                              (map #(map (partial get-in grid) %))
                              (map #(apply str %))))

        base (for [x (range (count (first grid)))
                   y (range (count grid))
                   :let [words (words-at-point x y)]]
               {:x x
                :y y
                :words words
                :hits (filter #(= "XMAS" %) words)})]
    (->> base
         (mapcat :hits)
         (count))))

(defn day-4-2 []
  (let [grid (->> (read-input "4.1")
                  (->grid))

        words-at-point (fn [x y]
                         (->> [[[(- y 1) (- x 1)]
                                [y x]
                                [(+ y 1) (+ x 1)]]
                               [[(+ y 1) (- x 1)]
                                [y x]
                                [(- y 1) (+ x 1)]]]

                              (map #(map (partial get-in grid) %))
                              (map #(apply str %))))

        base (for [x (range (count (first grid)))
                   y (range (count grid))
                   :let [words (words-at-point x y)]]
               {:x x
                :y y
                :words words
                :hit? (or (= ["MAS" "MAS"] words)
                          (= ["MAS" "SAM"] words)
                          (= ["SAM" "MAS"] words)
                          (= ["SAM" "SAM"] words))})]
    (->> base
         (filter :hit?)
         (count))))

(comment
  (day-4-1)
  (day-4-2)

  ;
  )

