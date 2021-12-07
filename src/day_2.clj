(ns day-2
    (:require [clojure.string :as string]))

;; Part 1
(defn part-1-navigate [commands]
      (reduce 
        (fn [[horz depth] command]
            ; (println command)
            (let [[cmdName cmdValue] (read-string (str "[:" command "]"))]
              (case cmdName
                :up [horz (- depth cmdValue)]
                :down [horz (+ depth cmdValue)]
                :forward [(+ horz cmdValue), depth])))
        [0 0]
        commands))

(defn part-1-test-data [args]
      (let [commands ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"]
            finalPos (part-1-navigate commands)
            [horz depth] finalPos]
           (println (* horz depth))))

(defn part-1-real-data [args]
      (let [commands (string/split-lines (slurp "data/day_2.txt"))
            finalPos (part-1-navigate commands)
            [horz depth] finalPos]
           (println finalPos)
           (println (* horz depth))))

;; Part 2
(defn part-2-navigate [commands]
      (reduce
        (fn [[horz depth aim] command]
            ; (println command)
            (let [[cmdName cmdValue] (read-string (str "[:" command "]"))]
                 (case cmdName
                          :up [horz depth (- aim cmdValue)]
                          :down [horz depth (+ aim cmdValue)]
                          :forward [(+ horz cmdValue) (+ depth (* aim cmdValue)) aim])))
        [0 0 0]
        commands))

(defn part-2-test-data [args]
      (let [commands ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"]
            finalPos (part-2-navigate commands)
            [horz depth aim] finalPos]
           (println (* horz depth))))

(defn part-2-real-data [args]
      (let [commands (string/split-lines (slurp "data/day_2.txt"))
            finalPos (part-2-navigate commands)
            [horz depth aim] finalPos]
           (println finalPos)
           (println (* horz depth))))

