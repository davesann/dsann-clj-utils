(ns utils.seq)

(defn partition-sequential 
  ([inc? [c1 c2 & others]]
    (partition-sequential inc? c1 c2 others c1 nil))
  ([inc? c1 c2 [c3 & others] result results]
    (if (nil? (or c1 c2))
      (reverse results)
      (if (= c2 (inc? c1))
        (recur inc? c2 c3 others result results)
        (recur inc? c2 c3 others c2 (cons (if (= result c1) c1 [result c1]) results))))))

(defn partition-same
  ([a-list]
    (partition-same a-list (first a-list) nil nil))
  ([a-list i result results]
    (if (empty? a-list)
      (reverse (cons result results))
      (let [i? (first a-list)
            r  (rest a-list)
            ]
        (if (= i i?)
          (recur r i  (cons i result)  results             )
          (recur r i? (list i?)        (cons result results)))))))


(defn r-compare 
  [v1 v2 [f & fns]]
  (if (nil? f)
    0
    (let [cmp-v (compare (f v1) (f v2))]
      (if (not (zero? cmp-v))
        cmp-v
        (recur v1 v2 fns)))))

(defn in [v a-seq]
  (some #{v} a-seq))

(defn join-seq [sep a-seq]
  (drop 1 (interleave (repeat sep) a-seq)))

;; count and sum
(defn count-if [f & colls]
  (reduce + (map #(if (f %) 1 0) (apply concat colls))))

(defn sum-f [f & colls]
  (reduce + (map f (apply concat colls))))

(defn sum-if [match? value-fn & colls]
  (reduce + (map #(if (match? %)
                    (value-fn %)
                    0) 
                 (apply concat colls))))
