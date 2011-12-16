(ns utils.core
  (:require
    [clojure.string :as s]
    [clojure.java.io :as io]
    [clojure.pprint :as pp]
    [clojure.tools.logging :as log]
    [clojure.contrib.math :as math]
    )
  (:import java.lang.Character)
  )


(defn round [v n]
  (let [f (math/expt 10 n)] 
    (/ (math/round (* f v)) f)))


(defn not-nil? [x]
  ((complement nil?) x))

(defn parse-number [s]
  "read a number to a double safely"
  (let [re  #"(?:^\d+$)|(?:^\d*[.]\d+)$"
        s-num (re-find re s)]
    (if s-num 
      (read-string s-num)
      nil)))




(defn mapvals [f a-map]
  (into {} 
        (map (fn [[k v]] [k (f v)])
         a-map)))

; legacy alias
(defn mapmap [f a-map]
  (mapvals f a-map))

(defn mapkeys [f a-map]
  (into {} 
        (map (fn [[k v]] [(f k) v])
         a-map)))

(defn mapkv [f g a-map]
  (into {} 
        (map (fn [[k v]] [(f k) (g v)])
         a-map)))

(defn remove-keys [p a-map]
  (into {} 
        (map (fn [[k v]]
               (if (not (p k))
                 [k v]))
             a-map)))

(defn remove-values [p a-map]
  (into {} 
        (map (fn [[k v]]
               (if (not (p v))
                 [k v]))
             a-map)))

(defn select-find [re a-map]
  (into {}
        (filter (fn [[k v]] (re-find re (name k))) a-map)))

(defn keys->keywords [a-map]
  (mapkeys keyword a-map))
                         
(defn select-prefixed [prefix a-map]
  (let [c (count prefix)]
    (into {}
          (remove nil?
                  (map (fn [[k v]] 
                         (let [n (name k)]
                           (if (.startsWith n prefix)
                             [(keyword (.substring n c)) v])))
                       a-map)))))                       


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


(defn apply-kf-list [a-map key-fn-map [k & key-list]]
  (if-let [f (key-fn-map k)]
    (let [v ( f a-map )
          new-m (if (nil? v) 
                  a-map 
                  (assoc a-map k v))]
      (if key-list
        (recur new-m key-fn-map key-list)
        new-m))
    
    ; else
    (throw (Exception. (str "config error: key " k " not found in key-fn map")))))
  

(defn map-c [a-map children-key key-fn-map key-list]
  {:pre
   [(or (map? a-map)
         (nil? a-map))
    (map? key-fn-map)
    (sequential? key-list)
    ]}
    
  (if (not (nil? a-map))
    (let [kids (a-map children-key)
          sub-results (cond
                        (nil? kids) 
                        nil
                        
                        (sequential? kids)
                        (map #(map-c % children-key key-fn-map key-list)
                             kids)
                        
                        (map? kids)
                        (mapmap #(map-c % children-key key-fn-map key-list)
                                  kids)
                        
                        true
                        (throw (Exception. "Cannot handle this type"))
                        )
                        
          n-map (assoc a-map children-key sub-results) 
          kvs (apply-kf-list n-map key-fn-map key-list)
          ]
      kvs)))
         
(defn hget- [a-map [k & ks]]
  "Hiererhcical map get"
  (if-let [v (a-map k)]
    (if ks
      (recur v ks)
      v)))

(defn hget [a-map ks]
  (hget- a-map (if (keyword? ks)
                 (map keyword (s/split (name ks) #"[.]"))
                 ks)))
  

(defn map->maplist [a-map id-key]
  (mapcat (fn [[k v-list]]
         (map #(assoc % id-key k) v-list))
       a-map))


(defn join-list [sep elements]
  (drop 1 
        (interleave (cycle [sep])
                    elements)))


(defn keyword->string [kw]
  (s/capitalize 
    (s/join " " (s/split (name kw) #"-"))))

(defn create-enum [a-list]
  (into {} (map-indexed 
             (fn [i v]
               [v {:enum i 
                   :value v
                   :text (keyword->string v)}])
             a-list)))


(defn abs [n]
  (if (neg? n) (- n) n))

(defn char-or-digit? [c]
  (java.lang.Character/isLetterOrDigit c))


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


