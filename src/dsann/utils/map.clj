(ns 
  ^{:doc 
    "utilities for maps"}
  
  dsann.utils.map
  (:require 
    [clojure.string :as string]
    [dsann.utils.seq :as useq]
    )
  )

;; Some useful map functions are found here

(defn mapvals [f a-map]
  "apply f to the values in the supplied map
   to produce a map {k1 (f v1) k2 (f v2) ...etc}"
  (into {} 
        (map (fn [[k v]] [k (f v)])
         a-map)))

(defn mapvals- [remove? f a-map]
  "apply f to the values in the supplied map
   to produce a map {k1 (f v1) k2 (f v2) ...etc}

  k (f v) will be excluded if (remove? (f v)) is true"
  (into {} 
    (useq/map- nil? 
      (fn [[k v]] 
        (let [v1 (f v)] 
          (if-noe (remove? v1) [k v1])))
      a-map)))

(defn mapkeys [f a-map]
  "apply f to the keys in the supplied map
   to produce a map {(f k1) v1 (f k2) v2 ...etc}"
  (into {} 
        (map (fn [[k v]] [(f k) v])
         a-map)))


(defn mapkvs [f g a-map]
  "apply f and g to the keys and values in the supplied map
   to produce a map {(f k1) (g v1) (f k2) (g v2) ... etc}"
  (into {} 
        (map (fn [[k v]] [(f k) (g v)])
         a-map)))


(defn mapvals-2 [f a-map]
  "similar to mapvals but f can use the key and value.
   apply f to the values in the supplied map
   to produce a map {k1 (f k1 v1) k2 (f k2 v2) ...etc}"
  (into {} 
        (map (fn [[k v]] [k (f k v)])
         a-map)))

(defn mapkeys-2 [f a-map]
  "similar to mapkeys but f can use the key and value.
   apply f to the values in the supplied map
   to produce a map {(f k1 v1) v1 (f k2 v2) v2 ... etc}"
  (into {} 
        (map (fn [[k v]] [(f k v) v])
         a-map)))

(defn mapkvs-2 [f g a-map]
  "similar to mapkvs but f and g can use the key and value.
   produce a map {(f k1 v1) (g k1 v1) (f k2 v2) (g k2 v2) ... etc}"
  (into {} 
        (map (fn [[k v]] [(f k v) (g k v)])
         a-map)))

(defn remove-keys [p? a-map]
  "remove entried {k v} from the map if (p? k) is true"
  (into {} 
        (map (fn [[k v]]
               (if-not (p k)
                 [k v]))
             a-map)))

(defn remove-vals [p? a-map]
  "remove entries {k v} from the map if (p? v) is true"
  (into {} 
        (map (fn [[k v]]
               (if-not (p? v)
                 [k v]))
             a-map)))

(defn apply-to-vals [f m]
  "apply f to values in m
   {k (apply f v) k2 (apply f v2) ...etc"
  (mapvals #(apply f %) m))

(defn concat-vals [m]
  "apply concat to values in m
   {k (apply concat v) k2 (apply concat v2) ...etc"
  (apply-to-vals concat m))



;; sort functions (convert to seq)
(defn sort-by-vals [f m]
  "convert a map to a seq of [k v] sorted by (f v)"
  (sort-by (fn [[k v]] (f v)) m))

(defn sort-by-keys [f m]
  "convert a map to a seq of [k v] sorted by (f k)"
  (sort-by (fn [[k v]] (f k)) m))

(defn sort-by-kvs [f m]
  "convert a map to a seq of [k v] sorted by (f k v)"
  (sort-by (fn [[k v]] (f k v)) m))


(defn map->maplist [a-map id-key]
  "if a-map is a 'group-by' map of maps
   {k  (m1 m2 m3)
    k2 (m5 m6)
    ...}
    this function produced
    (m1 m2 m3 m5 m6) where the respective k is assoc'ed as id-key into each map" 
  (mapcat (fn [[k v-list]]
            (map #(assoc % id-key k) v-list))
          a-map))



;; map sub selection
(defn select-vals [m & ks]
  (map #(% m) ks))

(defn select-best-val [better? m]
  "produces [k v] where v is (better? v v*) for all v in the map"
  (reduce
    (fn [[k1 v1] [k2 v2]]
      (if (better? v1 v2)
        [k1 v1]
        [k2 v2]))
    m))

(defn select-keys-re [re a-map]
  "select keys by re-find on (name k)"
  (into 
    {} (filter 
         (fn [[k v]] (re-find re (name k))) 
         a-map)))

(defn merge-as-list [& maps]
  "merges a list of maps (or a list of lists of key value pairs)
   where all the values are merged to a list.

(merge-as-list {:a 1 :b 2} {:a 2 :b [3]} {:c 2})
=> {:a (1 2), :b (2 [3]), :c (2)}
"
  (mapvals #(map second %) 
    (group-by first 
      (apply concat (map seq maps)))))



(defn dissoc2 [m k]
  "Alpha - adds dissoc capability for vectors and seqs
     slow but sometimes useful"
  (cond
    (map?    m) (dissoc m k)
    (vector? m) (vec (concat (subvec m 0 k) (subvec m (inc k))))
    (seq?    m) (concat (take k m) (drop (inc k) m))
    
    ;; exception
    )) 
    
;; taken from clojure.contrib.core
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure.

  modified to use dissoc2 applying to vectors and seqs
  "
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc2 m k)))
      m)
    (dissoc2 m k)))



(defn keys->keywords [a-map]
  (mapkeys keyword a-map))
          


; HMAP - heirarchical maps get and put need to tidy this up
; ------------------------------------
(defn split-key [k]
  "Split a . separated key into elements"
  (if (sequential? k) 
    k
    (let [ss (string/split (name k) #"\.")]
      (if (keyword? k) 
        (map keyword ss)
        ss))))

(defn unsplit-key [ks]
  (cond 
    (keyword? (first ks)) (keyword 
                            (string/join "."
                              (map name ks)))
    true (string/join "." ks)))

; modified from assoc-in core
(defn assoc-in2 [m [k & ks] v]
  "modified from core.assoc-in
  does not throw exceptions when overwriting values that are not maps"
  (if ks
    (let [t (get m k)
          next-m (if (map? t) t {})]
      (assoc m k (assoc-in2 next-m ks v)))
    (assoc m k v)
    ))

(defn hget
  ([a-map hkey] (hget a-map hkey nil))
  ([a-map hkey default]
    (let [[a-key & other-keys] (split-key hkey)
          d# default 
          v (get a-map a-key 'd#)
          ]
      (if other-keys
        (if (map? v)  
          (hget v other-keys default)
          default)
        (if (= v 'd#) 
          default 
          v)))))

(defn hput [m hkey v]
  (assoc-in2 m (split-key hkey) v))

(defn flatten-keys
  ([m] (flatten-keys {} [] m))
  ([a ks m]
    (if (map? m)
      (reduce into {}
        (map 
          (fn [[k v]] (flatten-keys a (conj ks k) v)) 
          (seq m)))
      (assoc a ks m))))

(defn flat-map [hmap]
  (into {} 
    (map
      (fn [[k v]] [(unsplit-key k) v]) 
      (seq (flatten-keys hmap)))))

(defn unflat-map [a-map]
  (let [f (fn
            ([] {})
            ([m kv] (hput m (key kv) (val kv))))] 
  (reduce f {} a-map)))

