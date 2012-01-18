(ns dsann.utils.x.math)

(defn cos [n]
  (. js/Math (cos n)))
  
(defn sin [n]
  (. js/Math (sin n)))
  
(defn atan2 [x y]
  (. js/Math (atan2 x y)))

(def PI (.-PI js/Math))

(defn pow [n e]
  (. js/Math (pow n e)))

(defn round1 [x]
  (. js/Math (round x)))

(defn round 
  ([x n]
    (if n
      (let [f (pow 10 n)]
        (/ (round (* f x)) f))
      (round1 x))))
    
(defn sqrt [n]
  (. js/Math (sqrt n)))
