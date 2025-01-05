(ns day17
  (:require
   [clojure.string :as str]
   [std :refer [->long read-input]]))

(def input-file
  "17.1")

(defn read-init-state []
  (let [lines (read-input input-file)]
    {:program (mapv ->long (re-seq #"\d+" (nth lines 4)))
     :output []
     :r {:a (->long (first (re-find #"(\d+)" (nth lines 0))))
         :b (->long (first (re-find #"(\d+)" (nth lines 1))))
         :c (->long (first (re-find #"(\d+)" (nth lines 2))))}
     :pc 0}))

(defn step [state]
  (update state :pc #(+ % 2)))

(defn combo [state operand]
  (cond
    (<= operand 3) operand
    (= operand 4) (get-in state [:r :a])
    (= operand 5) (get-in state [:r :b])
    (= operand 6) (get-in state [:r :c])))

(defn adv [state operand]
  (-> state
      (update-in [:r :a]
                 #(long (/ % (Math/pow 2 (combo state operand)))))
      (step)))

(defn bxl [state operand]
  (-> state
      (update-in [:r :b] #(bit-xor % operand))
      (step)))

(defn bst [state operand]
  (-> state
      (assoc-in [:r :b] (mod (combo state operand) 8))
      (step)))

(defn jnz [state operand]
  (if (= 0 (get-in state [:r :a]))
    (step state)
    (assoc-in state [:pc] operand)))

(defn bxc [state _operand]
  (-> state
      (update-in [:r :b] #(bit-xor % (get-in state [:r :c])))
      (step)))

(defn out [state operand]
  (-> state
      (update :output conj (mod (combo state operand) 8))
      (step)))

(defn bdv [state operand]
  (-> state
      (assoc-in [:r :b]
                (long (/ (get-in state [:r :a])
                         (Math/pow 2 (combo state operand)))))
      (step)))

(defn cdv [state operand]
  (-> state
      (assoc-in [:r :c]
                (long (/ (get-in state [:r :a])
                         (Math/pow 2 (combo state operand)))))
      (step)))

(def operators
  {0 adv
   1 bxl
   2 bst
   3 jnz
   4 bxc
   5 out
   6 bdv
   7 cdv})

(defn run-program [init-state]
  (loop [{:keys [pc program] :as state} init-state]
    (let [operator (get operators (get program pc))
          operand (get program (inc pc))]
      (if operator
        (recur (operator state operand))
        state))))

(defn output-of [a]
  (let [b (bit-and a 2r111)
        b (bit-xor b 3)
        c (bit-shift-right a b)
        b (bit-xor b 4)
        b (bit-xor b c)]
    (bit-and b 2r111)))

(defn gives-output [exp-output]
  (filter #(= exp-output (output-of %))
          (range 2r1000)))

(defn pt1 []
  (let [init-state (read-init-state)
        final-state (run-program init-state)]
    (->> final-state :output (str/join ","))))

(defn pt2 []
  (let [program (:program (read-init-state))

        quine-seeds
        (loop [[n & n'] (drop 1 (reverse program))
               candidates (gives-output (last program))]
          (if (nil? n)
            candidates
            (recur n'
                   (for [c candidates
                         i (range 2r1000)
                         :let [a (+ i (bit-shift-left c 3))]
                         :when (= n (output-of a))]
                     a))))]
    (first (sort quine-seeds))))

(comment
  (time (pt1))
  (time (pt2))
  ;
  )

