(ns utils.enums)

(defn create [a-seq]
  (when-let [s (seq a-seq)]
    (into {} (map-indexed 
               (fn [i v]
                 [v {:enum i 
                     :value v
                     :text (name keyword) 
                     }])
               s))))
