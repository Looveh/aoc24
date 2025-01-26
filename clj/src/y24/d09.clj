(ns y24.d09
  (:require [clojure.string :as str]
            [clj.std :as std]))

(defn parse-input []
  (->> (std/read-input 24 9)
       (first)
       (#(str/split % #""))
       (map std/->long)))

(def input (parse-input))

(defn pt1 []
  (letfn [(expand-input [input]
            (->> input
                 (partition-all 2)
                 (map-indexed (fn [idx [file-size empty-size]]
                                [(repeat file-size idx)
                                 (repeat (or empty-size 0) nil)]))
                 (flatten)
                 (vec)))

          (checksum [coll]
            (->> coll
                 (map-indexed #(* %1 %2))
                 (apply +)))

          (compress [coll]
            (loop [acc []
                   curr-idx 0
                   last-idx (dec (count coll))]
              (cond
                (> curr-idx last-idx)
                acc

                (nil? (nth coll last-idx))
                (recur acc curr-idx (dec last-idx))

                (nil? (nth coll curr-idx))
                (recur (conj acc (nth coll last-idx))
                       (inc curr-idx)
                       (dec last-idx))

                :else
                (recur (conj acc (nth coll curr-idx))
                       (inc curr-idx)
                       last-idx))))]

    (->> input
         expand-input
         compress
         checksum)))

(defn pt2 []
  (letfn [(expand-input [input]
            (->> input
                 (partition-all 2)
                 (map-indexed (fn [idx [file-size empty-size]]
                                [(repeat file-size idx)
                                 (repeat (or empty-size 0) nil)]))
                 (flatten)))

          (checksum [coll]
            (->> coll
                 (map-indexed (fn [idx fid]
                                (if fid
                                  (* idx fid)
                                  0)))
                 (apply +)))

          (replace-block [coll fid start end]
            (loop [coll' coll
                   idx start]
              (if (>= idx end)
                coll'
                (recur (assoc coll' idx fid)
                       (inc idx)))))

          (size-of-block-at-idx [coll idx]
            (loop [end (inc idx)]
              (cond
                (= end (count coll))
                (- end idx)

                (= (nth coll idx) (nth coll end))
                (recur (inc end))

                :else
                (- end idx))))

          (start-of-block-with-fid [coll fid]
            (loop [idx 0]
              (if (= fid (nth coll idx))
                idx
                (recur (inc idx)))))

          (start-of-first-empty-block-of-size [coll size]
            (loop [idx 0
                   start nil
                   size' 0]
              (cond
                (= size size')
                start

                (>= idx (count coll))
                start

                (nil? (nth coll idx))
                (recur (inc idx) (or start idx) (inc size'))

                :else
                (recur (inc idx) nil 0))))

          (fids-in-coll [coll]
            (->> coll
                 (distinct)
                 (filter some?)
                 (sort)))

          (compress [coll]
            (loop [coll' (vec coll)
                   [fid :as fids] (reverse (fids-in-coll coll))]
              (if (empty? fids)
                coll'
                (let [start (start-of-block-with-fid coll' fid)
                      size (size-of-block-at-idx coll' start)
                      start' (start-of-first-empty-block-of-size coll' size)]
                  (if (and start' (< start' start))
                    (recur (-> coll'
                               (replace-block fid start' (+ start' size))
                               (replace-block nil start (+ start size)))
                           (rest fids))
                    (recur coll' (rest fids)))))))]

    (->> input
         expand-input
         compress
         checksum)))

(println :pt1 (time (pt1)))
(println :pt2 (time (pt2)))
