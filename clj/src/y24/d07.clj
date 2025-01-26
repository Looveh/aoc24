(ns y24.d07
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clj.std :as std]))

(defn parse-line [line]
  (let [[result right] (str/split line #": ")
        operands (->> (str/split right #"\s+")
                      (map bigint))]
    [(bigint result) operands]))

(def input
  (->> (std/read-input 24 7)
       (map parse-line)))

(defn pt1 []
  (letfn [(calc [operands operators]
            (loop [acc (first operands)
                   operands' (rest operands)
                   operators' operators]
              (if (or (empty? operators') (empty? operands'))
                acc
                (let [operator (first operators')
                      operand (first operands')]
                  (recur (operator acc operand)
                         (rest operands')
                         (rest operators'))))))

          (assess-line [[result operands]]
            (let [operator-variants (combo/selections [+ *] (dec (count operands)))]
              (->> operator-variants
                   (map #(calc operands %))
                   (std/find-first #(= % result)))))]

    (->> input
         (map assess-line)
         (filter some?)
         (apply +))))

(defn pt2 []
  (letfn [(double-pipe [a b]
            (bigint (str a b)))

          (calc [operands operators]
            (loop [acc (first operands)
                   operands' (rest operands)
                   operators' operators]
              (if (or (empty? operators') (empty? operands'))
                acc
                (let [operator (first operators')
                      operand (first operands')]
                  (recur (operator acc operand)
                         (rest operands')
                         (rest operators'))))))

          (assess-line [[result operands]]
            (let [operator-variants (combo/selections [+ * double-pipe] (dec (count operands)))]
              (->> operator-variants
                   (map #(calc operands %))
                   (std/find-first #(= % result)))))]

    (->> input
         (map assess-line)
         (filter some?)
         (apply +))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
