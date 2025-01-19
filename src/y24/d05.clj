(ns y24.d05
  (:require [clojure.string :as str]
            [std :as std]))

(defn parse-input []
  (let [input (std/read-input 2024 5)
        [rules-strs _ updates-strs] (partition-by #(= "" %) input)]
    {:rules (map #(str/split % #"\|") rules-strs)
     :updates (map #(str/split % #",") updates-strs)}))

(def input (parse-input))

(defn pt1 []
  (let [rule-ok? (fn [u [l r]]
                   (or (not (and (contains? (set u) l)
                                 (contains? (set u) r)))
                       (< (std/idx-of u l)
                          (std/idx-of u r))))

        in-order? (fn [u]
                    (->> (for [r (:rules input)]
                           (rule-ok? u r))
                         (every? true?)))

        middle-page (fn [u]
                      (nth u (/ (count u) 2)))]

    (->> (:updates input)
         (filter in-order?)
         (map middle-page)
         (map std/->long)
         (apply +))))

(defn pt2 []
  (let [rule-ok? (fn [u [l r]]
                   (or (not (and (contains? (set u) l)
                                 (contains? (set u) r)))
                       (< (std/idx-of u l)
                          (std/idx-of u r))))

        in-order? (fn [u]
                    (->> (for [r (:rules input)]
                           (rule-ok? u r))
                         (every? true?)))

        middle-page (fn [u]
                      (nth u (/ (count u) 2)))

        into-order (fn [u]
                     (reduce
                      (fn [acc page]
                        (let [variations (for [i (range (inc (count acc)))]
                                           (std/insert-at-idx acc i page))]
                          (std/find-first in-order? variations)))
                      [(first u)]
                      (rest u)))]

    (->> (:updates input)
         (remove in-order?)
         (map into-order)
         (map middle-page)
         (map std/->long)
         (apply +))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
