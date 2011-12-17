(ns dsann.utils.x.math)

(defn cos [n]
  (java.lang.Math/cos (double n)))
  
(defn sin [n]
  (java.lang.Math/sin (double n)))
  
(defn atan2 [x y]
  (java.lang.Math/atan2 (double x) (double y)))

(def PI java.lang.Math/PI)

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
      (/ (round (* f x)) f))))

(defn sqrt [n]
  (java.lang.Math/sqrt n))
