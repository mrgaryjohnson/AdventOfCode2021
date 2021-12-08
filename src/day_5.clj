(ns day-5
    (:require [clojure.string :as string]))

(defn expand-straight-line [[from to]]
      (if (== (first from) (first to))
        (let [lowY (min (second from) (second to))
              highY (max (second from) (second to))]
             (map (fn [y] [(first from) y]) (range lowY (inc highY))))
        (let [lowX (min (first from) (first to))
              highX (max (first from) (first to))]
             (map (fn [x] [x (second from)]) (range lowX (inc highX))))))

(defn expand-diagonal-line [[from to]]
      (let [[fromX fromY] from
            [toX toY] to
            lowX (min fromX toX)
            highX (max fromX toX)
            xRange (range lowX (inc highX))
            xVals (if (> fromX toX) (reverse xRange) xRange)
            lowY (min fromY toY)
            highY (max fromY toY)
            yRange (range lowY (inc highY))
            yVals (if (> fromY toY) (reverse yRange) yRange)]
           (map vector xVals yVals)))
;; Part 1

(defn part-1-find-overlaps [lines]
      (->> lines
           (filter (fn [[to from]] (or (== (first to) (first from)) (== (second to) (second from)))))
           (map expand-straight-line)
           (apply concat)
           (group-by identity)
           (reduce (fn [counts [point instances]]
                       (assoc counts point (count instances)))
                   {})
           (filter #(>= (second %) 2))
           (map first)))

(defn part-1-test-data [args]
      (let [lines [[[0,9] [5,9]]
                   [[8,0] [0,8]]
                   [[9,4] [3,4]]
                   [[2,2] [2,1]]
                   [[7,0] [7,4]]
                   [[6,4] [2,0]]
                   [[0,9] [2,9]]
                   [[3,4] [1,4]]
                   [[0,0] [8,8]]
                   [[5,5] [8,2]]]
            overlaps (part-1-find-overlaps lines)]
           (println overlaps)
           (println (count overlaps))))

(defn part-1-real-data [args]
      (let [lines (map #(read-string (str "[[" (string/replace % #"->" "] [") "]]")) (string/split-lines (slurp "data/day_5.txt")))
            overlaps (part-1-find-overlaps lines)]
           (println overlaps)
           (println (count overlaps))))

;; Part 2
(defn expand-line [line]
      (let [[from to] line]
           (if (or (== (first to) (first from)) (== (second to) (second from)))
             (expand-straight-line line)
             (expand-diagonal-line line))))

(defn part-2-find-overlaps [lines]
      (->> lines
           (map expand-line)
           (apply concat)
           (group-by identity)
           (reduce (fn [counts [point instances]]
                       (assoc counts point (count instances)))
                   {})
           (filter #(>= (second %) 2))
           (map first)))

(defn part-2-test-data [args]
      (let [lines [[[0,9] [5,9]]
                   [[8,0] [0,8]]
                   [[9,4] [3,4]]
                   [[2,2] [2,1]]
                   [[7,0] [7,4]]
                   [[6,4] [2,0]]
                   [[0,9] [2,9]]
                   [[3,4] [1,4]]
                   [[0,0] [8,8]]
                   [[5,5] [8,2]]]
            overlaps (part-2-find-overlaps lines)]
           (println overlaps)
           (println (count overlaps))))

(defn part-2-real-data [args]
      (let [lines (map #(read-string (str "[[" (string/replace % #"->" "] [") "]]")) (string/split-lines (slurp "data/day_5.txt")))
            overlaps (part-2-find-overlaps lines)]
           (println overlaps)
           (println (count overlaps))))
