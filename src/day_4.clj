(ns day-4
    (:require [clojure.string :as string]))

(defn calc-score [[lastDraw board]]
      (let [unmarkedSum (reduce + 0 (filter #(< 0 %) (flatten board)))]
           (* unmarkedSum lastDraw)))

(defn winning-board? [board]
      (let [rowWin (some (fn [row] (every? #(> 0 %) row)) board)
            pivotedBoards (apply map vector board)
            colWin (some (fn [row] (every? #(> 0 %) row)) pivotedBoards)]
           (or rowWin colWin)))

;; Part 1
(defn part-1-find-first-winner [draws boards]
      (->> [boards draws nil]
           (iterate (fn [[boardsToMark, remainingDraws, lastDraw]]
                        (let [draw (first remainingDraws)
                              markedBoards (map (fn [board]
                                                    (map (fn [row]
                                                             (replace {draw -1} row))
                                                         board))
                                                boardsToMark)]
                             [markedBoards (rest remainingDraws) draw])))
           (drop-while #(not-any? winning-board? (first %)))  ;; Ignore rounds with no winners
           first  ;; Round with winner
           ((fn [[boards _ lastDraw]]
                (let [winningBoard (first (filter winning-board? boards))]
                     [lastDraw winningBoard])))))

(defn part-1-test-data [args]
      (let [draws [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
            boards [[[22 13 17 11  0]
                     [ 8  2 23  4 24]
                     [21  9 14 16  7]
                     [ 6 10  3 18  5]
                     [ 1 12 20 15 19]]
                    [[ 3 15  0  2 22]
                     [ 9 18 13 17  5]
                     [19  8  7 25 23]
                     [20 11 10 24  4]
                     [14 21 16 12  6]]
                    [[14 21 17 24  4]
                     [10 16 15  9 19]
                     [18  8 23 26 20]
                     [22 11 13  6  5]
                     [ 2  0 12  3  7]]]
            winner (part-1-find-first-winner draws boards)
            score (calc-score winner)]
           (println winner)
           (println score)))

(defn part-1-real-data [args]
      (let [lines (string/split-lines (slurp "data/day_4.txt"))
            groups (filter #(not (string/blank? (first %))) (partition-by string/blank? lines))
            draws (read-string (str "[" (ffirst groups) "]"))
            boards (map (fn [board] (map #(read-string (str "[" % "]")) board)) (rest groups))
            winner (part-1-find-first-winner draws boards)
            score (calc-score winner)]
           (println winner)
           (println score)))

;; Part 2
(defn part-2-find-last-winner [draws boards]
      (->> [boards draws nil]
           (iterate (fn [[boardsToMark, remainingDraws, lastDraw]]
                        (let [draw (first remainingDraws)
                              nonWinningBoards (filter (complement winning-board?) boardsToMark)
                              markedBoards (map (fn [board]
                                                    (map (fn [row]
                                                             (replace {draw -1} row))
                                                         board))
                                                nonWinningBoards)]
                             [markedBoards (rest remainingDraws) draw])))
           (drop-while #(or (> (count (first %)) 1) ;; Ignore rounds with multiple boards
                            (not (winning-board? (ffirst %)))))  ;; Ignore rounds where last board is not a winner
           first  ;; Round with last winner
           ((fn [[boards _ lastDraw]]
              [lastDraw (first boards)]))))

(defn part-2-test-data [args]
      (let [draws [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
            boards [[[22 13 17 11  0]
                     [ 8  2 23  4 24]
                     [21  9 14 16  7]
                     [ 6 10  3 18  5]
                     [ 1 12 20 15 19]]
                    [[ 3 15  0  2 22]
                     [ 9 18 13 17  5]
                     [19  8  7 25 23]
                     [20 11 10 24  4]
                     [14 21 16 12  6]]
                    [[14 21 17 24  4]
                     [10 16 15  9 19]
                     [18  8 23 26 20]
                     [22 11 13  6  5]
                     [ 2  0 12  3  7]]]
            winner (part-2-find-last-winner draws boards)
            score (calc-score winner)]
           (println winner)
           (println score)))

(defn part-2-real-data [args]
      (let [lines (string/split-lines (slurp "data/day_4.txt"))
            groups (filter #(not (string/blank? (first %))) (partition-by string/blank? lines))
            draws (read-string (str "[" (ffirst groups) "]"))
            boards (map (fn [board] (map #(read-string (str "[" % "]")) board)) (rest groups))
            winner (part-2-find-last-winner draws boards)
            score (calc-score winner)]
           (println winner)
           (println score)))
