(ns aoc24.main
  (:require [clojure.core.matrix :as cm]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]]
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

(defn remove-nth [n coll]
  (keep-indexed (fn [idx item]
                  (when (not= idx n)
                    item))
                coll))

(defn idx-of [coll value]
  (first (keep-indexed (fn [idx v]
                         (when (= v value)
                           idx))
                       coll)))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn insert-at-idx [coll idx val]
  (concat (take idx coll)
          [val]
          (drop idx coll)))

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
  (let [safe? (fn [line]
                (and (or (= line (sort line))
                         (= (reverse line) (sort line)))
                     (->> (partition 2 1 line)
                          (every? (fn [[a b]]
                                    (<= 1 (abs (- a b)) 3))))))]
    (->> (read-input "2.1")
         (map ->cols)
         (map (partial map ->int))
         (filter safe?)
         (count))))

(defn day-2-2 []
  (let [safe? (fn [line]
                (and (or (= line (sort line))
                         (= (reverse line) (sort line)))
                     (->> (partition 2 1 line)
                          (every? (fn [[a b]]
                                    (<= 1 (abs (- a b)) 3))))))
        safe'? (fn [line]
                 (let [permutations (for [i (range (count line))]
                                      (remove-nth i line))]
                   (some safe? permutations)))]

    (->> (read-input "2.1")
         (map ->cols)
         (map (partial map ->int))
         (filter safe'?)
         (count))))

(comment
  (day-2-1)
  (day-2-2)
  ;
  )

;; ---------------------------------------------------------------------------
;; Day 3

(defn day-3-1 []
  (let [mul-line (fn [line]
                   (->> line
                        (re-seq #"mul\((\d+),(\d+)\)")
                        (map (fn [[_ a b]]
                               (* (->int a) (->int b))))
                        (apply +)))]
    (->> (read-input "3.1")
         (map mul-line)
         (apply +))))

(defn day-3-2 []
  (->> (read-input "3.1")
       (apply str)
       (#(str/split % #"do\(\)"))
       (map #(str/split % #"don't\(\).*$"))
       (map first)
       (apply str)
       (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[_ a b]]
              (* (->int a) (->int b))))
       (apply +)))

(comment
  (day-3-1)
  (day-3-2)
  ;
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

;; ---------------------------------------------------------------------------
;; Day 5

(defn day-5-1 []
  (let [input (read-input "5.1")
        [rules-strs _ updates-strs] (partition-by #(= "" %) input)
        rules (map #(str/split % #"\|") rules-strs)
        updates (map #(str/split % #",") updates-strs)

        rule-ok?
        (fn [u [l r]]
          (or (not (and (contains? (set u) l)
                        (contains? (set u) r)))
              (< (idx-of u l)
                 (idx-of u r))))

        in-order?
        (fn [u]
          (->> (for [r rules]
                 (rule-ok? u r))
               (every? true?)))

        middle-page
        (fn [u]
          (nth u (/ (count u) 2)))]

    (->> updates
         (filter in-order?)
         (map middle-page)
         (map ->int)
         (apply +))))

(defn day-5-2 []
  (let [input (read-input "5.1")
        [rules-strs _ updates-strs] (partition-by #(= "" %) input)
        rules (map #(str/split % #"\|") rules-strs)
        updates (map #(str/split % #",") updates-strs)

        rule-ok?
        (fn [u [l r]]
          (or (not (and (contains? (set u) l)
                        (contains? (set u) r)))
              (< (idx-of u l)
                 (idx-of u r))))

        in-order?
        (fn [u]
          (->> (for [r rules]
                 (rule-ok? u r))
               (every? true?)))

        middle-page
        (fn [u]
          (nth u (/ (count u) 2)))

        into-order
        (fn [u]
          (reduce
           (fn [acc page]
             (let [variations (for [i (range (inc (count acc)))]
                                (insert-at-idx acc i page))]
               (find-first in-order? variations)))
           [(first u)]
           (rest u)))]

    (->> updates
         (remove in-order?)
         (map into-order)
         (map middle-page)
         (map ->int)
         (apply +))))

(comment
  (day-5-1)
  (day-5-2)
  ;
  )
