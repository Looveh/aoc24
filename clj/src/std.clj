(ns std
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [is]]
            [grid :as grid]
            [lambdaisland.dotenv :as dotenv]))

(def env
  (dotenv/parse-dotenv (slurp "../.env")))

(defn input-path
  ([year day]
   (input-path year day false))
  ([year day example?]
   (str "../inputs/" year "/" day (when example? "x") ".txt")))

(defn download-input [year day]
  (let [session (env "AOC_SESSION")
        url (str "https://adventofcode.com/20" year "/day/" day "/input")
        response (http/get url {:cookies {"session" {:value session}}})]
    (spit (input-path year day) (:body response))))

(defn slurp-input
  ([year day]
   (slurp-input year day false))
  ([year day example?]
   (let [path (input-path year day example?)]
     (when (not (.exists (io/file path)))
       (download-input year day))
     (slurp path))))

(defn read-input
  ([year day]
   (read-input year day false))
  ([year day example?]
   (->> (slurp-input year day example?)
        (str/split-lines))))

(defn read-grid
  ([year day]
   (read-grid year day false))
  ([year day example?]
   (grid/str->Grid (slurp-input year day example?))))

(defn ->long [s]
  (Long/parseLong s))

(defn find-first [f coll]
  (first (filter f coll)))

(defn largest-clique
  "Using Bron-Kerbosh.

   Expects graph of shape {:node-1 #{:node-2} :node-2 #{:node-1}"
  [graph]
  (loop [stack (list [(set (keys graph)) #{} #{} #{}])
         result #{}]
    (if (empty? stack)
      result
      (let [[candidates excluded r _] (first stack)
            rest-stack (rest stack)]
        (if (and (empty? candidates)
                 (empty? excluded))
          (if (> (count r) (count result))
            (recur rest-stack r)
            (recur rest-stack result))
          (let [pivot (first (concat candidates excluded))
                non-neighbors (set/difference candidates (get graph pivot))
                new-stack (concat
                           (for [v non-neighbors]
                             [(set/intersection candidates (get graph v))
                              (set/intersection excluded (get graph v))
                              (conj r v)
                              result])
                           rest-stack)]
            (recur new-stack result)))))))

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

(defn insert-at-idx [coll idx val]
  (concat (take idx coll)
          [val]
          (drop idx coll)))

(defn ->grid [lines]
  (vec (mapv #(str/split % #"") lines)))

(defn grid->graph [comparator grid]
  (letfn [(edges [[x y]]
            (->> [[-1 0] [1 0] [0 -1] [0 1]]
                 (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
                 (filter (fn [[x' y']]
                           (and (>= x' 0)
                                (< x' (count (first grid)))
                                (>= y' 0)
                                (< y' (count grid))
                                (comparator (get-in grid [y x]) (get-in grid [y' x'])))))

                 (set)))]
    (reduce (fn [graph [x y]]
              (assoc graph [x y] {:coord [x y]
                                  :val (get-in grid [y x])
                                  :edges (edges [x y])}))
            (sorted-map)
            (for [x (range (count (first grid)))
                  y (range (count grid))]
              [x y]))))

(defn graph-path [graph from to]
  (loop [q [[from]]
         visited #{}]
    (if (empty? q)
      nil
      (let [[p & q'] q
            n (last p)]
        (if (= n to)
          p
          (if (visited n)
            (recur q' visited)
            (recur (into q' (map #(conj p %) (get-in graph [n :edges])))
                   (conj visited n))))))))

(defn graph-paths [graph from to]
  (loop [q [[from]]
         ps []]
    (if (empty? q)
      ps
      (let [[p & q'] q
            n (last p)]
        (if (= n to)
          (recur q' (conj ps p))
          (recur (reduce (fn [acc n']
                           (if (some #(= n' %) p)
                             acc
                             (conj acc (conj p n'))))
                         q'
                         (get-in graph [n :edges]))
                 ps))))))

(defn ->cols [line]
  (str/split line #"\s+"))

(defn flatten-1 [coll]
  (mapcat identity coll))

(defn graph-cheapest-path
  {:test (fn []
           (let [graph {:a {:b 2, :c 2}
                        :b {:c 3, :d 1, :a 6}
                        :c {:d 5}
                        :d {}}]
             (is (= (graph-cheapest-path graph :a :d)
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

(defn vec+ [[x y] [x' y']]
  [(+ x x') (+ y y')])

(defn vec- [[x y] [x' y']]
  [(- x x') (- y y')])


