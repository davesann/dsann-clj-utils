(in-ns 'utils.math)

(import 'java.lang.Math)

(require '[utils.x.core :as u])

(defn pow [n e]
  (java.lang.Math/pow n e))

(defn round 
  ([x]
    (try
      (java.lang.Math/round x)
      (catch Exception e
        (java.lang.Math/round (double x)))))
  ([x n]
    (let [f (pow 10 n)]
      (/ (round (* f x)) f))) 
  )

(defn sqrt [n]
  (java.lang.Math/sqrt n))

(defn square [n]
  (* n n))

(defn rms [vector]
  (sqrt (reduce + (map square vector))))
  
(defn euclidian [v1 v2]
  (rms (map - v2 v1)))