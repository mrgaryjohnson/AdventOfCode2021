(ns day-1)

;; Part 1
(defn part-1-count-increases [depths]
  (reduce
    (fn [[count last-depth] next-depth]
      (let [new-count (if (and last-depth (> next-depth last-depth))
                        (+ 1 count)
                        count)]
        [new-count next-depth]))
    [0, nil]
    depths))

(defn part-1-test-data [args]
  (let [increases (part-1-count-increases [199 200 208 210 200 207 240 269 260 263])]
       (print (first increases))))

(defn part-1-real-data [args]
  (let [lines (slurp "data/day_1.txt")
        depths (read-string (str "[" lines "]"))
        increases (part-1-count-increases depths)]
       (print (first increases))))

;; Part 2
(defn part-2-count-increases [depths]
 (reduce
   (fn [[count prev-depth prev-2-depth prev-3-depth] next-depth]
     (let [new-count (if (and prev-3-depth (> next-depth  prev-3-depth))
                       (+ 1 count)
                       count)]
       [new-count next-depth prev-depth prev-2-depth]))
   [0, nil, nil, nil]
   depths))

(defn part-2-test-data [args]
      (let [increases (part-2-count-increases [199 200 208 210 200 207 240 269 260 263])]
           (print (first increases))))

(defn part-2-real-data [args]
      (let [lines (slurp "data/day_1.txt")
            depths (read-string (str "[" lines "]"))
            increases (part-2-count-increases depths)]
           (print (first increases))))
