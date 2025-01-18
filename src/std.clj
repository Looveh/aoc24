(ns std
  (:require
   [clojure.string :as str]
   [clj-http.client :as http]
   [clojure.java.io :as io]
   [clojure.edn :as edn]))

(def env
  (edn/read-string (slurp "env.edn")))

(defn input-path [year day & {:keys [example?]}]
  (str "inputs/" year "/" day (when example? "x") ".txt"))

(defn download-input [year day]
  (let [session (:aoc-session env)
        url (str "https://adventofcode.com/" year "/day/" day "/input")
        response (http/get url {:cookies {"session" {:value session}}})]
    (spit (input-path year day) (:body response))))

(defn slurp-input [year day & {:keys [example?]}]
  (let [path (input-path year day example?)]
    (when (not (.exists (io/file path)))
      (download-input year day))
    (slurp path)))

(defn read-input [year day & {:keys [example?]}]
  (->> (slurp-input year day :example example?)
       (str/split-lines)))

(defn ->long [s]
  (Long/parseLong s))



