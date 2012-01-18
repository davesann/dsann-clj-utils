(ns 
  ^{:doc "some partition functions"}
  dsann.utils.partition
  (:require 
    [dsann.utils.seq :as useq]
    ))

(def partition-same (partial partition-by identity))

(defn partition-by-reduce 
  "partitions with tak-wile-reduce
    for example usage - see partition-by-width"
  
  [reduce-fn pred? coll]
  (when-let [s (seq coll)]
    (lazy-seq
      (let [[fst & rst] s
            init (reduce-fn fst)
            run (useq/take-while-reduce init reduce-fn pred? rst)
            remainder (drop (count run) rst)]
        (cons (cons fst run) (partition-by-reduce reduce-fn pred? remainder))))))


(defn partition-by-width 
  "partition as (p1 p2 p3 ...) where for each p,
     (sum (count-fn item) p) is the maximum value possible less than width
   
     count-fn specifes the 'length' of each p  
  
     this is a 'greedy line break'
  " 
  
  [count-fn width coll]
  (partition-by-reduce 
    (fn
      ([i] (count-fn i))
      ([rv i] (+ rv (count-fn i))))
    (fn [rv i] (< rv width))
    coll))


(defn partition-inc 
  "partition into sequences where
    the elements of each s are incremental according to the fn inc

  e.g (partition-inc inc [1 2 3 5 6 7 ...]) 
    => ((1 2 3) (5 6 7 ...) ...)
  "
  [inc-fn coll]
  (when-let [s (seq coll)]
    (partition-by-reduce
      (fn 
        ([i] i)
        ([rv i] (inc-fn rv)))
      (fn [rv i] (= rv i))
      s)))

                           
(defn take-inc 
  "take 'incremental' values, returning the pair [last value remainder]
    e.g 
      (take-inc inc  1 [1 2 3 5 6 7 9 1 2 3])
        => [3 (5 6 7 9 1 2 3)]

      (up/take-inc dec 3 [3 2 1 0 -1 3 5])
        [-1 (3 5)]
  " 
  ([inc-fn v coll] (take-inc inc-fn v coll nil)) 
  ([inc-fn v coll last-val]
    (if-let [s (seq coll)]
      (let [[fst & rst] coll]
        (if (= v fst)
          (recur inc-fn (inc-fn v) rst fst)
          [last-val s]
          ))
      [last-val nil])))


(defn partition-inc-compressed
  "as partition-inc but 'compresses' the result to ranges
  
   e.g (partition-inc-compressed inc [1 2 3 5 6 7 9 1 2 3])
      -> ([1 3] [5 7] 9 [1 3])
"
  ([inc-fn coll]
    (lazy-seq 
      (when-let [s (seq coll)]
        (let [[f & r] s
              [last-i remainder] (take-inc inc-fn (inc-fn f) r nil)
              result (if last-i [f last-i] f)
              ]
          (cons result (partition-inc-compressed inc-fn remainder))))))) 
