(ns 
  ^{:doc "general sequence utils"}
  dsann.utils.seq)

;; compound compare
(defn compound-compare 
  "compare 
    (cmp1 v1 v2)
    (cmp2 v1 v2)
    ...
   
   returns value if any returns none-zero or ultimately 0 (same) 
  "
  [[f & fns] v1 v2]
  (if (nil? f)
    0
    (let [cmp-v (compare (f v1) (f v2))]
      (if-not (zero? cmp-v)
        cmp-v
        (recur fns v1 v2)))))

(defn compound-compare-fn [& fns]
  "creates a compound-compare function cmp-f that can be used as
    (cmp-f v1 v2)"
  (partial compound-compare fns)) 

;; map and remove in one
(defn map- [remove? f & colls]
  "map removing values for which (remove? (f v)) is true"
  (remove remove? (apply (partial map f) colls)))
              
(defn mapcat- [remove? f & colls]
  "mapcat removing values for which (remove? (f v)) is true"
  (apply concat 
    (apply (partial map- remove? f) colls))) 
  
(defn map-indexed-
  "map indexed removing items if (remove? (f i v)) is true"
  [remove? f coll]
  (remove remove? (map-indexed f coll)))

;; predicates
(defn in?
  "v in seq"
  [a-seq v]
  (some #{v} a-seq))

(defn not-in?
  "v not in seq"
  [v a-seq]
  (not (in? v a-seq)))


(defn starts-with [a-seq sub-seq]
  "true if a-seq starts with the seq sub-seq"
  (if (not (seq sub-seq))
    true
    (let [fs  (first a-seq)
          fss (first sub-seq)]
      (if (= fs fss)
        (recur (rest a-seq) (rest sub-seq))
        false))))



;; miscellaneous 
(defn join-seq
  "join a seq with a separator sep"
  [sep a-seq]
  (drop 1 (interleave (repeat sep) a-seq)))

(defn ensure-sequential 
  "ensure that you have a sequence"
  [v] 
  (if (sequential? v) v (list v)))

; conversions to map
(defn into-by 
  "convert ( e1 e2 e3 ...) to
    { (f e1) e1 
      (f e2) e2 
      (f e3) e3 ... }"
  [m f a-seq]
  (into m (map (fn [i] [(f i) i]) a-seq)))


; search 
(defn find-first
  "find first element, e, for which (pred? e) is true"
  [pred? a-seq]
  (first (drop-while (complement pred?) a-seq)))


;; index filter and remove operations
(defn filter-by-index-pred 
  "filter (include) indexes for which (pred? i) is true"
  [pred? a-seq]
  (map-indexed- nil? 
                (fn [i v]
                  (if (pred? i) v))
                a-seq))

(defn remove-by-index-pred 
  "remove indexes for which (pred? i) is true"
  [pred? a-seq]
  (filter-by-index-pred (complement pred?) a-seq))
  
(defn filter-by-index 
  "filter (include) indexes in the set idx-set"
  [idx-set a-seq]
  (filter-by-index-pred #(idx-set %) a-seq))

(defn remove-by-index [idx-set a-seq ]
  "remove indexes in the set idx-set"
  (remove-by-index-pred #(idx-set %) a-seq))


(defn take-while-reduce 
  "fancy take accumulating a reduced value on taken items
    this value can then be tested in the take fn

   e.g (take-while-reduce 0 (fn [v i] (inc v)) 
                            (fn [v i] (= v i)) 
                          [1 2 3 5 6 7])
   => (1 2 3)"
  [initial reduce-fn pred coll]
  (when-let [s (seq coll)]
    (let [[f & r] coll
          reduce-value (reduce-fn initial f)]
      (if (pred reduce-value f)
        (cons f (take-while-reduce reduce-value reduce-fn pred r))))))

(comment
  ;; examples
(take-while-reduce 0 (fn [v i] (inc v)) (fn [v i] (= v i)) [1 2 3 5 6 7])

(take-while-reduce 0 
                   (fn [v i] (+ v (count i)))
                   (fn [v i] (< v 20)) 
                   (ccsu/partition  "the quick brown fox jumps" #"\s+"))
)

