(ns dsann.utils.thread-conditionals)

(defn if> [v bool f & stuff]
  "use in (->... constructions

   e.g (-> a-map
         (if> (test? something) conj a b c d)
         ...)"
  (if bool
    (apply (partial f v) stuff)))


(defn if-not> [v bool f & stuff]
  "use in (->... constructions

   e.g (-> a-map
         (if> (test? something) conj a b c d)
         ...)"
  (if-not bool
    (apply (partial f v) stuff)))

(defn cons-if> [a-seq bool v]
  "use in (->... constructions cons if bool is true"
  (if bool
    (cons v a-seq)
    a-seq))

(defn cons-if-not> [a-seq bool v]
  "use in (->... constructions cons if bool is false"
  (if-not bool
    (cons v a-seq)
    a-seq))

(defn conj-if> [a-seq bool & stuff]
  "use in (->... constructions conj if bool is true
   you can use if> for similar effect too"
  (if bool
    (apply (partial conj a-seq) stuff)
    a-seq))

(defn conj-if-not> [a-seq bool & stuff]
  "use in (->... constructions conj if bool is false
   you can use if-not> for similar effect too"
  
  (if-not bool
    (apply (partial conj a-seq) stuff)
    a-seq))

(defn assoc-if> [m bool & kvs]
  "for (-> m ... assocs if bool is true
   you can use if> for similar effect too"
  (if bool
    (apply (partial assoc m) kvs)
    m))

(defn assoc-if-not> [m bool & kvs]
  "for (-> m ... assocs if bool is false
   you can use if-not> for similar effect too"
  (if-not bool
    (apply (partial assoc m) kvs)
    m))
