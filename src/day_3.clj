(ns day-3
    (:require [clojure.string :as string]))

(defn report-num-to-digits [reportNum]
      (let [charVec (vec reportNum)
            strSeq (map str charVec)]
        (map read-string strSeq)))

(defn decode-binary [digits]
      (reduce #(+ %2 (* 2 %1)) 0 digits))

(defn invert-digit [digit]
      (nth [1 0] digit))

;; Part 1

(defn part-1-calc-gamma [reportNums]
      (let [countCutoff (/ (count reportNums) 2)
            digitsSeq (map report-num-to-digits reportNums)
            counts (reduce (fn [running next]
                               (if running
                                 (map + running next)
                                 next))
                           nil
                           digitsSeq)]
           (map #(if (> % countCutoff) 1 0) counts)))

(defn part-1-calc-epsilon [gammaDig]
      (map invert-digit gammaDig))

(defn part-1-test-data [args]
      (let [reportNums ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"]
            gammaDig (part-1-calc-gamma reportNums)
            epsilonDig (part-1-calc-epsilon gammaDig)
            power (* (decode-binary gammaDig) (decode-binary epsilonDig))]
           (println gammaDig)
           (println epsilonDig)
           (println power)))

(defn part-1-real-data [args]
      (let [reportNums (string/split-lines (slurp "data/day_3.txt"))
            gammaDig (part-1-calc-gamma reportNums)
            epsilonDig (part-1-calc-epsilon gammaDig)
            power (* (decode-binary gammaDig) (decode-binary epsilonDig))]
           (println gammaDig)
           (println epsilonDig)
           (println power)))

;; Part 2

(defn get-most-common-digit [digitSeq digitPos]
      (let [cutoff (/ (count digitSeq) 2)
            digits (map #(nth % digitPos) digitSeq)
            ones (count (filter #(> % 0) digits))]
           (if (>= ones cutoff) 1 0)))

(defn get-least-common-digit [digitSeq digitPos]
      (invert-digit (get-most-common-digit digitSeq digitPos)))

(defn part-2-calc-o2 [reportNums]
      (let [digitsSeq (map report-num-to-digits reportNums)]
           (->> [0 digitsSeq]  ; Start with all nums, looking at first digit
                (iterate (fn [[bitPos nums]]
                             (let [mostCommonDigit (get-most-common-digit nums bitPos)
                                   filteredNums (filter #(== (nth % bitPos) mostCommonDigit) nums)]
                                  [(inc bitPos) filteredNums])))  ; Next time, look at next digit, and filtered nums
                (drop-while #(> (count (second %)) 1))  ; ignore vectors with more than one num
                first  ; vector with only one num
                second  ; sequence of one num
                first)))  ; num

(defn part-2-calc-co2 [reportNums]
      (let [digitsSeq (map report-num-to-digits reportNums)]
           (->> [0 digitsSeq]  ; Start with all nums, looking at first digit
                (iterate (fn [[bitPos nums]]
                             (let [mostCommonDigit (get-least-common-digit nums bitPos)
                                   filteredNums (filter #(== (nth % bitPos) mostCommonDigit) nums)]
                                  [(inc bitPos) filteredNums])))  ; Next time, look at next digit, and filtered nums
                (drop-while #(> (count (second %)) 1))  ; ignore vectors with more than one num
                first  ; vector with only one num
                second  ; sequence of one num
                first)))  ; num

(defn part-2-test-data [args]
      (let [reportNums ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"]
            o2Rating (part-2-calc-o2 reportNums)
            co2Rating (part-2-calc-co2 reportNums)
            lifeSupportRating (* (decode-binary o2Rating) (decode-binary co2Rating))]
           (println o2Rating)
           (println co2Rating)
           (println lifeSupportRating)))

(defn part-2-real-data [args]
      (let [reportNums (string/split-lines (slurp "data/day_3.txt"))
            o2Rating (part-2-calc-o2 reportNums)
            co2Rating (part-2-calc-co2 reportNums)
            lifeSupportRating (* (decode-binary o2Rating) (decode-binary co2Rating))]
           (println o2Rating)
           (println co2Rating)
           (println lifeSupportRating)))
