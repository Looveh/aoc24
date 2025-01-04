(ns graph
  (:require [clojure.test :refer [is]]))

(defn find-cheapest-path
  {:test (fn []
           (let [graph {:a {:b 2, :c 2}
                        :b {:c 3, :d 1, :a 6}
                        :c {:d 5}
                        :d {}}]
             (is (= (find-cheapest-path graph :a :d)
                    [3 [:a :b :d]]))))}
  [graph start end]
  (loop [costs {start 0}
         paths {start [start]}
         queue (sorted-map 0 #{start})]
    (if (empty? queue)
      nil
      (let [[current-cost node-set] (first queue)
            current (first node-set)
            remaining-set (disj node-set current)
            queue' (if (empty? remaining-set)
                     (dissoc queue current-cost)
                     (assoc queue current-cost remaining-set))]
        (if (= current end)
          [current-cost (get paths current)]
          (let [edges (get graph current {})
                updates (for [[neighbor edge-cost] edges
                              :let [new-cost (+ current-cost edge-cost)]
                              :when (or (not (contains? costs neighbor))
                                        (< new-cost (get costs neighbor)))]
                          [neighbor new-cost (conj (get paths current) neighbor)])
                new-costs (into costs (map (fn [[n c _]] [n c]) updates))
                new-paths (into paths (map (fn [[n _ p]] [n p]) updates))
                new-queue (reduce (fn [q [n c _]]
                                    (update q c (fnil conj #{}) n))
                                  queue'
                                  updates)]
            (recur new-costs new-paths new-queue)))))))

(defn find-all-cheapest-paths
  {:test (fn []
           (let [graph {:a {:b 2, :c 2}
                       :b {:c 3, :d 1, :a 6}
                       :c {:d 1}
                       :d {}}]
             (is (= (find-all-cheapest-paths graph :a :d)
                    #{[3 [:a :b :d]]
                      [3 [:a :c :d]]}))))}
  [graph start end]
  (loop [costs {start 0}
         paths {start #{[start]}}
         queue (sorted-map 0 #{start})]
    (if (empty? queue)
      nil
      (let [[current-cost node-set] (first queue)
            current (first node-set)
            remaining-set (disj node-set current)
            queue' (if (empty? remaining-set)
                    (dissoc queue current-cost)
                    (assoc queue current-cost remaining-set))]
        (if (= current end)
          (set (map #(vector current-cost %) (get paths current)))
          (let [edges (get graph current {})
                updates (for [[neighbor edge-cost] edges
                            :let [new-cost (+ current-cost edge-cost)]
                            :when (or (not (contains? costs neighbor))
                                    (= new-cost (get costs neighbor)))]
                        [neighbor new-cost (map #(conj % neighbor) (get paths current))])
                new-costs (into costs (map (fn [[n c _]] [n c]) updates))
                new-paths (reduce (fn [ps [n _ new-ps]]
                                  (update ps n (fnil into #{}) new-ps))
                                paths
                                updates)
                new-queue (reduce (fn [q [n c _]]
                                  (update q c (fnil conj #{}) n))
                                queue'
                                updates)]
            (recur new-costs new-paths new-queue)))))))
