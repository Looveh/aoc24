(ns day19
  (:require
   [clojure.string :as s]
   [std :refer [read-input]]))

(def towel-arrangement-count
  (memoize
   (fn [design towels]
     (if (empty? design)
       1
       (let [sub-design-counts
             (for [towel towels
                   :when (s/starts-with? design towel)]
               (towel-arrangement-count (subs design (count towel))
                                        towels))]
         (reduce + sub-design-counts))))))

(defn parse-input []
  (let [lines (read-input "19.1")
        towels (s/split (first lines) #", ")
        designs (drop 2 lines)]
    [towels designs]))

(defn pt1 []
  (let [[towels designs] (parse-input)]
    (->> designs
         (filter #(not= 0 (towel-arrangement-count % towels)))
         (count))))

(defn pt2 []
  (let [[towels designs] (parse-input)]
    (->> designs
         (map #(towel-arrangement-count % towels))
         (reduce +))))

(comment
  (time (pt1))
  (time (pt2))
  ;
  )


