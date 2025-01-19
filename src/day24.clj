(ns day24
  (:require
   [clojure.string :as str]
   [std :as std]))

(def input
  (std/read-input 2024 24))

(defn gate-and [in1 in2]
  (cond
    (or (nil? in1) (nil? in2)) nil
    (and (= 1 in1) (= 1 in2)) 1
    :else 0))

(defn gate-or [in1 in2]
  (cond
    (or (nil? in1) (nil? in2)) nil
    (or (= 1 in1) (= 1 in2)) 1
    :else 0))

(defn gate-xor [in1 in2]
  (cond
    (or (nil? in1) (nil? in2)) nil
    (= in1 in2) 0
    :else 1))

(def ops
  {"AND" gate-and
   "OR" gate-or
   "XOR" gate-xor})

(def gates
  (->> (nth (partition-by #(= "" %) input) 2)
       (map (fn [line]
              (let [[in1 op in2 _ out] (str/split line #" ")]
                {:in1 in1
                 :in2 in2
                 :op (ops op)
                 :out out})))
       (vec)))

(def init-ports
  (let [ports (atom (->> gates
                         (mapcat (fn [gate]
                                   [(:in1 gate) (:in2 gate) (:out gate)]))
                         (reduce (fn [acc port]
                                   (assoc acc port nil)) (sorted-map))))]
    (doseq [line (first (partition-by #(= "" %) input))]
      (let [[port v] (str/split line #": ")]
        (swap! ports assoc port (std/->long v))))
    @ports))

(defn build-answer [ports]
  (let [z-ports (vals (filter (fn [[port _]]
                                (str/starts-with? port "z"))
                              ports))]
    (when (every? some? z-ports)
      (str/join "" (reverse z-ports)))))

(defn run [gates ports]
  (let [ports (atom ports)]
    (while (not (build-answer @ports))
      (doseq [gate gates]
        (let [in1 (get @ports (:in1 gate))
              in2 (get @ports (:in2 gate))
              out (:out gate)
              op (:op gate)]
          (when (and (some? in1) (some? in2))
            (swap! ports assoc out (op in1 in2))))))
    (Long/parseLong (build-answer @ports) 2)))

(defn pt1 []
  (run gates init-ports))

(println :pt1 (time (pt1)))


