(ns utils.map
  (:require 
    [clojure.string :as string]
    [utils.seq :as useq]
    )
  )

(defn assoc-if [pred m k v]
  (if (pred v)
    (assoc m k v)
    m))

(defn assoc-if-not-nil [m k v]
  (assoc-if #(not (nil? %)) m k v))

(defn assoc-if-not-empty [m k v]
  (assoc-if #(not (empty? %)) m k v))

(defn select-vals [m & ks]
  (map #(% m) ks))

(defn select-best [better? m]
  (reduce
    (fn [[k1 v1] [k2 v2]]
      (if (better? v1 v2)
        [k1 v1]
        [k2 v2]))
    m))


(defn mapvals [f a-map]
  (into {} 
        (map (fn [[k v]] [k (f v)])
         a-map)))

(defn mapvals- [remove? f a-map]
  (into {} 
    (useq/map- nil? 
      (fn [[k v]] 
        (let [v1 (f v)] 
          (if (remove? v1)  nil  [k v1])))
      a-map)))

;; these don't really shorten anything...
(defn mapvals-empty [f a-map]
  (mapvals- empty? f a-map))

(defn mapvals-nil [f a-map]
  (mapvals- nil? f a-map))

(defn mapkeys [f a-map]
  (into {} 
        (map (fn [[k v]] [(f k) v])
         a-map)))


(defn mapkvs [f g a-map]
  (into {} 
        (map (fn [[k v]] [(f k) (g v)])
         a-map)))


(defn mapvals-2 [f a-map]
  (into {} 
        (map (fn [[k v]] [k (f k v)])
         a-map)))

(defn mapkeys-2 [f a-map]
  (into {} 
        (map (fn [[k v]] [(f k v) v])
         a-map)))

(defn mapkvs-2 [f g a-map]
  (into {} 
        (map (fn [[k v]] [(f k v) (g k v)])
         a-map)))

(defn remove-keys [p a-map]
  (into {} 
        (map (fn [[k v]]
               (if (not (p k))
                 [k v]))
             a-map)))

(defn remove-vals [p a-map]
  (into {} 
        (map (fn [[k v]]
               (if (not (p v))
                 [k v]))
             a-map)))

(defn apply-to-vals [f m]
  (mapvals #(apply f %) m))

(defn concat-vals [m]
  (apply-to-vals concat m))


(defn select-find [re a-map]
  (into {}
        (filter (fn [[k v]] (re-find re (name k))) a-map)))

(defn keys->keywords [a-map]
  (mapkeys keyword a-map))
          
(defn map->maplist [a-map id-key]
  (mapcat (fn [[k v-list]]
         (map #(assoc % id-key k) v-list))
       a-map))

(defn sort-by-vals [f m]
  (sort-by (fn [[k v]] (f v)) m))

(defn sort-by-keys [f m]
  (sort-by (fn [[k v]] (f k)) m))

(defn sort-by-kvs [f m]
  (sort-by (fn [[k v]] (f k v)) m))

(defn merge-as-list [& maps]
  "merges a list of maps (or a list of lists of key value pairs)
   where all the values are merged to a list.

(merge-as-list {:a 1 :b 2} {:a 2 :b [3]} {:c 2})
=> {:a (1 2), :b (2 [3]), :c (2)}
"
  (mapvals #(map second %) 
    (group-by first 
      (apply concat (map seq maps)))))

; HMAP - heirarchical maps get and put
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

