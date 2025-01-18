(ns day23
  (:require [clojure.string :as str]
            [std :as std]))

(def input
  (std/read-input 2024 23))

(def graph
  (let [edges (map #(str/split % #"-") input)
        empty-graph (reduce (fn [acc [n1 n2]]
                              (-> acc
                                  (assoc n1 #{})
                                  (assoc n2 #{})))
                            (sorted-map)
                            edges)]
    (reduce (fn [graph [a b]]
              (-> graph
                  (update a conj b)
                  (update b conj a)))
            empty-graph
            edges)))

(defn interconnected? [n1 n2 n3]
  (and (contains? (get graph n1) n2)
       (contains? (get graph n1) n3)
       (contains? (get graph n2) n1)
       (contains? (get graph n2) n3)
       (contains? (get graph n3) n1)
       (contains? (get graph n3) n2)))

(defn interconnected-threes []
  (let [nodes (vec (keys graph))]
    (for [i (range 0 (count nodes))
          j (range (inc i) (count nodes))
          k (range (inc j) (count nodes))
          :let [n1 (nth nodes i)
                n2 (nth nodes j)
                n3 (nth nodes k)]
          :when (and (or (str/starts-with? n1 "t")
                         (str/starts-with? n2 "t")
                         (str/starts-with? n3 "t"))
                     (interconnected? n1 n2 n3))]
      [n1 n2 n3])))

(defn pt1 []
  (count (interconnected-threes)))

(defn pt2 []
  (let [subgraph (std/largest-clique graph)]
    (str/join "," (sort subgraph))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))

