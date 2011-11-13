(ns utils.x.parse-numbers)




(defn parse-integer [string & opts]
  (let [{:keys [radix junk-allowed],
         :or {radix 10, junk-allowed false},
         :as opts}
        (apply hash-map opts)]
    (try
     (Integer/parseInt string radix)
     (catch NumberFormatException e
       (when-not junk-allowed
         (throw NumberFormatException e))))))



(defn parse-int 
  ([s] (java.lang.Integer/parseInt s))
  ([s base]
    (java.lang.Integer/parseInt s base)))

(defn str16->int [s]
  (parse-int s 16))