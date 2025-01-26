(ns y24.d24
  (:require [clojure.string :as str]
            [clj.std :as std]))

(def input
  (std/read-input 24 24))

(defn and-gate [in1 in2]
  (cond
    (or (nil? in1) (nil? in2)) nil
    (and (= 1 in1) (= 1 in2)) 1
    :else 0))

(defn or-gate [in1 in2]
  (cond
    (or (nil? in1) (nil? in2)) nil
    (or (= 1 in1) (= 1 in2)) 1
    :else 0))

(defn xor-gate [in1 in2]
  (cond
    (or (nil? in1) (nil? in2)) nil
    (= in1 in2) 0
    :else 1))

(def ops
  {"AND" and-gate
   "OR" or-gate
   "XOR" xor-gate})

(def gates
  (->> (nth (partition-by #(= "" %) input) 2)
       (map (fn [line]
              (let [[in1 op in2 _ out] (str/split line #" ")]
                {:in1 in1
                 :in2 in2
                 :op op
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
            (swap! ports assoc out ((get ops op) in1 in2))))))
    (Long/parseLong (build-answer @ports) 2)))

(defn input-gate? [gate]
  (when gate
    (or (and (str/starts-with? (:in1 gate) "x")
             (str/starts-with? (:in2 gate) "y"))
        (and (str/starts-with? (:in1 gate) "y")
             (str/starts-with? (:in2 gate) "x")))))

(defn output-gate? [gate]
  (when gate
    (str/starts-with? (:out gate) "z")))

(defn input-idx [gate]
  (if-not (input-gate? gate)
    nil
    (std/->long (re-find #"\d+" (:in1 gate)))))

(defn output-idx [gate]
  (if-not (output-gate? gate)
    nil
    (std/->long (re-find #"\d+" (:out gate)))))

(defn next-gates [gate]
  (filter (fn [g]
            (or (= (:in1 g) (:out gate))
                (= (:in2 g) (:out gate))))
          gates))

(defn or-gate? [gate]
  (= "OR" (:op gate)))

(defn xor-gate? [gate]
  (= "XOR" (:op gate)))

(defn and-gate? [gate]
  (= "AND" (:op gate)))

(def okay-gate?
  (memoize
   (fn [gate]
     (cond
       (= "z00" (:out gate)) true
       (= "z01" (:out gate)) true
       (= "z45" (:out gate)) true
       (= "qtf" (:out gate)) true

       (and (output-gate? gate)
            (xor-gate? gate))
       true

       (and (input-gate? gate)
            (output-gate? gate)
            (= (input-idx gate) (output-idx gate))
            (< (input-idx gate) 1))
       true

       (and (input-gate? gate)
            (xor-gate? gate)
            (= 2 (count (next-gates gate)))
            (= #{"XOR" "AND"} (set (map :op (next-gates gate)))))
       true

       (and (and-gate? gate)
            (= 1 (count (next-gates gate)))
            (= #{"OR"} (set (map :op (next-gates gate)))))
       true

       (and (or-gate? gate)
            (= 2 (count (next-gates gate)))
            (= #{"AND" "XOR"} (set (map :op (next-gates gate)))))
       true

       (and (or-gate? gate)
            (= 1 (count (next-gates gate)))
            (= #{"XOR"} (set (map :op (next-gates gate)))))
       true

       :else false))))

(defn pt1 []
  (run gates init-ports))

(defn pt2 []
  (->> (for [gate gates
             :when (not (okay-gate? gate))]
         (:out gate))
       (sort)
       (str/join ",")))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
