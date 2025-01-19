(ns y24.d12
  (:require [std :as std]))

(def input
  (->> (std/read-input 2024 12)
       (std/->grid)))

(def graph
  (->> input
       (std/grid->graph =)))

(def proto-clusters
  (->> graph
       (vals)
       (group-by :val)
       (vals)
       (map (fn [nodes]
              (reduce (fn [acc node]
                        (assoc acc (:coord node) node))
                      (sorted-map)
                      nodes)))))

(def clusters
  (mapcat
   (fn [graph]
     (loop [clusters []
            [node & nodes] (vals graph)]
       (if (not node)
         clusters
         (let [in-cluster?
               (std/find-first (fn [cluster]
                                 (std/graph-path graph
                                                 (:coord node)
                                                 (->> cluster first :coord)))
                               clusters)]
           (if in-cluster?
             (recur (conj (remove #(= in-cluster? %) clusters)
                          (conj in-cluster? node))
                    nodes)
             (recur (conj clusters [node])
                    nodes))))))
   proto-clusters))

(defn peripheral-edges [node]
  (let [[x y] (:coord node)]
    (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
               :let [x' (+ x dx)
                     y' (+ y dy)]]
           (when-not (contains? (:edges node) [x' y'])
             [[x y] [x' y']]))
         (filter some?))))

(defn nodes-adjacent? [[x y] [x' y']]
  (or (and (= x x') (= 1 (abs (- y y'))))
      (and (= y y') (= 1 (abs (- x x'))))))

(defn edges-adjacent? [[f t] [f' t']]
  (and (nodes-adjacent? f f')
       (nodes-adjacent? t t')))

(defn with-peripheral-edges [cluster]
  (map (fn [node]
         (assoc node :peripheral-edges (peripheral-edges node)))
       cluster))

(defn cluster-sides [edges]
  (loop [[edge & edges'] edges
         clusters #{}]
    (if (not edge)
      clusters
      (let [matching-cluster
            (std/find-first (fn [cluster]
                              (some #(edges-adjacent? % edge) cluster))
                            clusters)]
        (if matching-cluster
          (recur edges' (conj (remove #(= matching-cluster %) clusters)
                              (conj matching-cluster edge)))
          (recur edges' (conj clusters [edge])))))))

(defn sides [cluster]
  (->> cluster
       (mapcat :peripheral-edges)
       (cluster-sides)
       (count)))

(defn pt1 []
  (let [measure
        (fn [cluster]
          (let [area (count cluster)
                perimeter (->> cluster
                               (map #(- 4 (count (:edges %))))
                               (apply +))]
            (* area perimeter)))]

    (->> clusters
         (map measure)
         (apply +))))

(defn pt2 []
  (let [measure
        (fn [cluster]
          (let [area (count cluster)]
            (* area (sides cluster))))]

    (->> clusters
         (map with-peripheral-edges)
         (map measure)
         (apply +))))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
