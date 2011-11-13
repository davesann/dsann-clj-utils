(ns utils.enums)


(defn create-enum [a-list]
  (into {} (map-indexed 
             (fn [i v]
               [v {:enum i 
                   :value v
                   :text (name keyword) ;(keyword->string v)
                   }])
             a-list)))
