(ns utils.misc)

;(load "x/core")

(defn not-nil? [x]
  ((complement nil?) x))


;;------------
;; won't work in CLJS
(comment
(defn parse-number [s]
  "read a number to a double safely"
  (let [re  #"(?:^\d+$)|(?:^\d*[.]\d+)$"
        s-num (re-find re s)]
    (if s-num 
      (read-string s-num)
      nil)))


(defn select-prefixed [prefix a-map]
  (let [c (count prefix)]
    (into {}
          (remove nil?
                  (map (fn [[k v]] 
                         (let [n (name k)]
                           (if (.startsWith n prefix)
                             [(keyword (subs n c)) v])))
                       a-map)))))                       


         
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
  
)
