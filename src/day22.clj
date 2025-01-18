(ns day22
  (:require [std :as std]))

(def input
  #_[1 10 100 2024]
  #_[1 2 3 2024]
  (map std/->long (std/read-input 2024 22)))

(defn calc-secret-number [sn]
  (let [sn (bit-xor sn (bit-shift-left sn 6))
        sn (bit-and sn 2r111111111111111111111111)
        sn (bit-xor sn (bit-shift-right sn 5))
        sn (bit-and sn 2r111111111111111111111111)
        sn (bit-xor sn (bit-shift-left sn 11))
        sn (bit-and sn 2r111111111111111111111111)]
    sn))

(defn pt1 []
  (->> (for [n input]
         (->> (iterate calc-secret-number n)
              (drop 2000)
              (first)))
       (apply +)))

(defn pt2 []

  ; please cover your eyes

  (let [windows (atom [])
        best-window (atom nil)
        window-prices (atom {})
        counts (atom {})
        res (atom 0)
        get-price (fn [prices i]
                    (mod (nth prices (- i 0)) 10))]
    (println :a)
    (doseq [num input
            :let [num (atom num)
                  prices (atom [])]]
      (reset! window-prices {})
      (dotimes [i 2000]
        (let [price (mod @num 10)]
          (swap! prices conj price)
          (swap! num calc-secret-number)
          (when (>= i 4)
            (let [window [(- (get-price @prices (- i 3))
                             (get-price @prices (- i 4)))
                          (- (get-price @prices (- i 2))
                             (get-price @prices (- i 3)))
                          (- (get-price @prices (- i 1))
                             (get-price @prices (- i 2)))
                          (- (get-price @prices (- i 0))
                             (get-price @prices (- i 1)))]]
              (when (not (contains? @window-prices window))
                (swap! window-prices assoc window price))))))
      (swap! windows conj @window-prices))
    (println :b (count @windows))
    (doseq [k (->> @windows (mapcat keys) distinct)]
      (doseq [window @windows]
        (swap! counts assoc k (+ (get @counts k 0) (get window k 0)))))
    (println :c (count @counts) (ffirst @counts))
    (let [bananas (atom 0)]
      (doseq [[k v] @counts]
        (when (> v @bananas)
          (reset! bananas v)
          (reset! best-window k))))
    (println :d @best-window)
    (doseq [window @windows]
      (swap! res + (get window @best-window 0)))
    @res))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))

