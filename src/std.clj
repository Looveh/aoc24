(ns std
  (:require
   [clojure.string :as str]))

(defn slurp-input [filename]
  (slurp (str "inputs/2024/" filename ".txt")))

(defn read-input [filename]
  (str/split-lines (slurp-input filename)))

(defn ->long [s]
  (Long/parseLong s))
