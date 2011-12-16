(ns utils.hmap
  (:require [clojure.string :as string])
  )

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

