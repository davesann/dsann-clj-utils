(ns utils.map-c
  (:require [utils.map :as um])
  )

;; only non generic because of exceptions

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
                        (um/mapvals #(map-c % children-key key-fn-map key-list)
                                    kids)
                        
                        true
                        (throw (Exception. "Cannot handle this type"))
                        )
                        
          n-map (assoc a-map children-key sub-results) 
          kvs (apply-kf-list n-map key-fn-map key-list)
          ]
      kvs)))