(ns std
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def env
  (edn/read-string (slurp "env.edn")))

(defn input-path
  ([year day]
   (input-path year day false))
  ([year day example?]
   (str "inputs/" year "/" day (when example? "x") ".txt")))

(defn download-input [year day]
  (let [session (:aoc-session env)
        url (str "https://adventofcode.com/" year "/day/" day "/input")
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

