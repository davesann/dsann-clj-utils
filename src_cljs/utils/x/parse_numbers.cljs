(ns dsann.utils.x.parse-numbers)

(defn parse-int 
  ([s r] 
    (let [r (or r 10)]
      (js/parseInt s r))))

(defn str16->int [s]
  (parse-int s 16))
