(ns day-6
    (:require [medley.core :as m]))

;; Part 1

(defn part-1-process-days [initState, days]
      (->> [initState days]
           (iterate (fn [[oldState daysLeft]]
                        (let [newFish (map (constantly 8) (filter #(== % 0) oldState))
                              updatedFish (map #(if (== % 0) 6 (dec %))
                                               oldState)
                              newState (concat updatedFish newFish)]
                             [newState (dec daysLeft)])))
           (drop-while #(> (second %) 0))  ;; Ignore rounds where there are days left to process
           first  ;; The round with zero days left
           first))

(defn part-1-test-data [args]
      (let [initState [3,4,3,1,2]
            finalState (part-1-process-days initState 80)]
           (println (count finalState))))

(defn part-1-real-data [args]
      (let [lines (slurp "data/day_6.txt")
            initState (read-string (str "[" lines "]"))
            finalState (part-1-process-days initState 80)]
           (println (count finalState))))

;; Part 2

(defn part-2-process-days [initDayCounts days]
      (->> [initDayCounts days]
           (iterate (fn [[oldCounts daysLeft]]
                        (let [newCounts (reduce (fn [counts [day count]]
                                                    (let [daysToInc (if ( == day 0) [6 8] [(dec day)])]
                                                         (reduce (fn [c d] (update c d #(+ count (or % 0))))
                                                                 counts
                                                                 daysToInc)))
                                                {}
                                                oldCounts)]
                             [newCounts, (dec daysLeft)])))
           (drop-while #(> (second %) 0))  ;; Ignore rounds where there are days left to process
           first  ;; The round with zero days left
           first))

(defn part-2-test-data [args]
      (let [initState [3,4,3,1,2]
            initDayCounts (frequencies initState)
            finalCounts (part-2-process-days initDayCounts 256)
            total (reduce + 0 (map second finalCounts))]
           (println finalCounts)
           (println total)))

(defn part-2-real-data [args]
      (let [lines (slurp "data/day_6.txt")
            initState (read-string (str "[" lines "]"))
            initDayCounts (frequencies initState)
            finalCounts (part-2-process-days initDayCounts 256)
            total (reduce + 0 (map second finalCounts))]
           (println finalCounts)
           (println total)))
