(ns y24.d24
  (:require
   [clojure.set :as set]
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
                 :op op #_(ops op)
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

(defn pt1 []
  (run gates init-ports))

(defn collect [ports prefix]
  (let [target (vals (filter (fn [[port _]]
                               (str/starts-with? port prefix))
                             ports))]
    (when (every? some? target)
      (str/join "" (reverse target)))))

(def target-x
  (str/split (collect init-ports "x") #""))

(def target-y
  (str/split (collect init-ports "y") #""))

(def target
  (Long/toString
   (+ (Long/parseLong (str/join "" target-x) 2)
      (Long/parseLong (str/join "" target-y) 2))
   2))

(defn gates-leading-to [port]
  (loop [[curr & stack] (cons port (list))
         gates' #{}]
    (if-not curr
      gates'
      (let [gate (first (filter (fn [gate]
                                  (= curr (:out gate)))
                                gates))]

        (if gate
          (recur (conj stack (:in1 gate) (:in2 gate))
                 (conj gates' gate))
          (recur stack gates'))))))

(println :pt1 (time (pt1)))

(defn input-gate? [gate]
  (when gate
    (or (and (str/starts-with? (:in1 gate) "x")
             (str/starts-with? (:in2 gate) "y"))
        (and (str/starts-with? (:in1 gate) "y")
             (str/starts-with? (:in2 gate) "x")))))

(defn output-gate? [gate]
  (when gate
    (str/starts-with? (:out gate) "z")))

(defn gate-with-out-port [port]
  (first (filter (fn [gate]
                   (= port (:out gate)))
                 gates)))

(defn prev-gates [gate]
  (let [a (first (filter (fn [parent?]
                           (= (:in1 gate) (:out parent?)))
                         gates))
        b (first (filter (fn [parent?]
                           (= (:in2 gate) (:out parent?)))
                         gates))]
    (if (and a b)
      [a b]
      [])))

(defn input-idx [gate]
  (if-not (input-gate? gate)
    nil
    (std/->long (re-find #"\d+" (:in1 gate)))))

(defn output-idx [gate]
  (if-not (output-gate? gate)
    nil
    (std/->long (re-find #"\d+" (:out gate)))))

(defn prev-input-gate [gate]
  (first (filter input-gate? (prev-gates gate))))

(defn prev-non-input-gate [gate]
  (first (remove input-gate? (prev-gates gate))))

(def okay-gate?
  (memoize
   (fn [gate level]
     (cond
       (and (input-gate? gate)
            (output-gate? gate)
            (= (input-idx gate) (output-idx gate)))
       true

       (and (input-gate? gate)
            (= "AND" (:op gate))
            (= level (input-idx gate)))
       true

       (and (input-gate? gate)
            (= "XOR" (:op gate))
            (= level (input-idx gate)))
       true

       (and (output-gate? gate)
            (= "XOR" (:op gate))
            (= level (output-idx gate))
            (or (and (okay-gate? (first (prev-gates gate))
                                 level)
                     (okay-gate? (second (prev-gates gate))
                                 (dec level)))
                (and (okay-gate? (first (prev-gates gate))
                                 (dec level))
                     (okay-gate? (second (prev-gates gate))
                                 level))
                (and
                 (okay-gate? (prev-input-gate gate) level)
                 (okay-gate? (-> gate prev-non-input-gate prev-input-gate) (dec level)))))
       true

       (and (= "OR" (:op gate))
            (okay-gate? (prev-input-gate gate) level)
            (okay-gate? (-> gate prev-non-input-gate prev-input-gate) level))
       true

       (and (= "AND" (:op gate))
            (or (and (okay-gate? (first (prev-gates gate))
                                 level)
                     (okay-gate? (second (prev-gates gate))
                                 (dec level)))
                (and (okay-gate? (first (prev-gates gate))
                                 (dec level))
                     (okay-gate? (second (prev-gates gate))
                                 level))
                (and
                 (okay-gate? (prev-input-gate gate) level)
                 (okay-gate? (-> gate prev-non-input-gate prev-input-gate) (dec level))))
            #_(okay-gate? (prev-input-gate gate) level)
            #_(okay-gate? (-> gate prev-non-input-gate prev-input-gate) (dec level)))
       true

       :else false))))

(defn gate-idx [gate]
  (cond
    (output-gate? gate) (output-idx gate)
    (input-gate? gate) (input-idx gate)
    (= "OR" (:op gate)) (input-idx (prev-input-gate gate))
    (= "AND" (:op gate)) (input-idx (prev-input-gate gate))
    (= "XOR" (:op gate)) (input-idx (prev-input-gate gate))))

(defn all-pairs [elements]
  (if (not= (count elements) 8)
    (throw (IllegalArgumentException. "Must provide exactly 8 elements"))
    (for [a (range 8)
          b (range (inc a) 8)
          c (range 8)
          d (range (inc c) 8)
          e (range 8)
          f (range (inc e) 8)
          g (range 8)
          h (range (inc g) 8)
          :let [pairs [[(nth elements a) (nth elements b)]
                       [(nth elements c) (nth elements d)]
                       [(nth elements e) (nth elements f)]
                       [(nth elements g) (nth elements h)]]]
          :when (= (sort (flatten pairs))
                   (sort elements))]
      pairs)))

#_(def candidates
    (set (for [gate gates
               :let [n (gate-idx gate)]
               :when (not (okay-gate? gate n))]
           gate)))

#_(->>
   (set (for [gate gates
              :let [n (gate-idx gate)]
              :when (not (okay-gate? gate n))]
          gate))
   (map :out)
   (sort)
   (str/join ","))

(defn swap-out-ports [gates [port-1 port-2]]
  (println :swap-out-ports port-1 port-2)
  (let [gate-1-idx (std/idx-of gates (gate-with-out-port port-1))
        gate-2-idx (std/idx-of gates (gate-with-out-port port-2))]
    (-> gates
        (assoc-in [gate-1-idx :out] port-2)
        (assoc-in [gate-2-idx :out] port-1))))

#_(okay-gate?' (gate-with-out-port "z05") 5)
#_(input-idx (gate-with-out-port "z05"))

#_(okay-gate?' (gate-with-out-port "z01") 1)

#_(count (all-pairs (map :out candidates)))

(defn pt2 []
  (let [candidates (set (for [gate gates
                              :let [n (gate-idx gate)]
                              :when (not (okay-gate? gate n))]
                          gate))]
    (for [[p1 p2 p3 p4 :as pairs] (all-pairs (map :out candidates))
          :let [_ (println :pairs pairs)
                gates (-> gates
                          (swap-out-ports p1)
                          (swap-out-ports p2)
                          (swap-out-ports p3)
                          (swap-out-ports p4))]
          :when (= target (run gates init-ports))]
      pairs)))

#_(println :pt2 (time (pt2)))

(for [gate gates
      :let [n (gate-idx gate)]
      :when (not (okay-gate? gate n))]
  [ gate n])

#_(->>
 (for [gate gates
       :let [n (gate-idx gate)]
       :when (not (okay-gate? gate n))]
   gate)
 (map :out)
 (sort)
 (str/join ","))

#_(gate-idx (gate-with-out-port "kpp"))

(prev-gates (gate-with-out-port "sms"))
(prev-gates (gate-with-out-port "qnv"))
(okay-gate? (gate-with-out-port "sms") 30)

; djn,kpp,rqf,sgj,vss,wbv,z36,z45
; djn,kpp,rqf,sgj,vss,wbv,z36,z45


