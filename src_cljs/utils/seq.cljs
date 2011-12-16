(ns utils.seq
  (:require 
    [utils.x.core :as u]
    )
  )

;(require 
;  '[utils.x.core :as u])

(defn map-by [f a-seq]
  "creates a map {(f e) e} for each element in the list
   last-value wins on same key
  "
  (into {}
    (map (fn [e] [(f e) e]) a-seq)))

(defn ensure-sequential [v] 
  (if (sequential? v)
    v
    [v]))


(defn take-while-reduce [initial reduce-fn pred coll]  
  (when-let [s (seq coll)]
    (let [[f & r] coll
          reduce-value (reduce-fn initial f)]
      (if (pred reduce-value f)
        (cons f (take-while-reduce reduce-value reduce-fn pred r))))))


(comment
(take-while-reduce 0 (fn [v i] (inc v)) (fn [v i] (= v i)) [1 2 3 5 6 7])

(take-while-reduce 0 
                   (fn [v i] (+ v (count i)))
                   (fn [v i] (< v 20)) 
                   (ccsu/partition  "the quick brown fox jumps" #"\s+"))
)

(defn partition-by-reduce [reduce-fn pred coll] 
  (when-let [s (seq coll)]
    (lazy-seq
      (let [[fst & rst] s
            init (reduce-fn fst)
            run (take-while-reduce init reduce-fn pred rst)
            remainder (drop (count run) rst)]
        (cons (cons fst run) (partition-by-reduce reduce-fn pred remainder))))))


(defn partition-by-width [count-fn width coll]
  "partition as (p1 p2 p3 ...) where for each p,
     (sum (count-fn item) p) is the maximum value possible less than width
   
   this is a 'greedy line break'
  " 
  (partition-by-reduce 
    (fn
      ([i] (count-fn i))
      ([rv i] (+ rv (count-fn i))))
    (fn [rv i] (< rv width))
    coll))


(defn partition-inc [inc-fn coll]
  (when-let [s (seq coll)]
    (partition-by-reduce
      (fn 
        ([i] i)
        ([rv i] (inc-fn rv)))
      (fn [rv i] (= rv i))
      s)))
                           
(comment
  (def s (mapcat #(range 0 %) (iterate (fn [_] (rand-int 10)) 0 )))
)

(defn take-inc [inc-fn v coll last-val]
  (if-let [s (seq coll)]
    (let [[fst & rst] coll]
      (if (= v fst)
        (recur inc-fn (inc-fn v) rst fst)
        [last-val s]
        ))
    [last-val nil]))

(defn partition-inc-compressed
  ([inc-fn coll]
    (lazy-seq 
      (when-let [s (seq coll)]
        (let [[f & r] s
              [last-i remainder] (take-inc inc-fn (inc-fn f) r nil)
              result (if last-i [f last-i] f)
              ]
          (cons result (partition-inc-compressed inc-fn remainder))))))) 
  
(def partition-same (partial partition-by identity))

(defn map- [remove? f & colls]
  "map removing values for which (remove? (f v)) is true"
  (remove remove? (apply (partial map f) colls)))
              
(defn mapcat- [remove? f & colls]
  "map removing values for which (remove? (f v)) is true"
  (apply concat 
    (apply (partial map- remove? f) colls))) 
  
(defn r-compare 
  [[f & fns] v1 v2]
  (if (nil? f)
    0
    (let [cmp-v (compare (f v1) (f v2))]
      (if (not (zero? cmp-v))
        cmp-v
        (recur fns v1 v2)))))

(defn r-compare-fn [& fns]
  (partial r-compare fns)) 



(defn tabulate-overlap-acc 
  ([indexed-seq held iresult fresult]
    (if (empty? indexed-seq)
      (if (empty? held)
        (conj fresult iresult)
        (recur held [] [] (conj fresult iresult)))
      (let [[item & remainder] indexed-seq
            [[s e] v] item
            [n-held new-indexed-seq] (split-with (fn [[[s1 _] _]] (> e s1)) remainder)
           
            _x (doall n-held) ; obscure bug in clojurescript
            
            ]
        (recur new-indexed-seq (concat held n-held) (conj iresult v) fresult)
        ))))

(defn tabulate-overlap
  "given an indexed sequence [[[from to] value] [[from to] value] ...]
creates a list of lists where no index spans overlap in each row.

from values are inclusive 
to values are exclusive
"
  ([indexed-seq] (tabulate-overlap-acc indexed-seq [] [] [])))
  




(defn in [a-seq v]
  (some #{v} a-seq))

(defn not-in [v a-seq]
  (not (in v a-seq)))

(defn join-seq [sep a-seq]
  (drop 1 (interleave (repeat sep) a-seq)))




(defn unit-seq? [a & r]
  (empty? r))  

(defn unit-seq->unit [s]
  (if (sequential? s)
    (let [[i & r] s]
      (if (empty? r)
        i
        s))
    s))



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
