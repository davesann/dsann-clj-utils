(ns dsann.utils.math
  (:require [dsann.utils.x.math :as xm]))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn average 
  "Finds the average of the numbers entered"
  [& all]
  (/ (apply + all) (count all)))

(defn square [n]
  (* n n))

(defn clamp [min-x max-x x]
  (cond 
    (< x min-x)
    min-x
    
    (> x max-x)
    max-x
    
    true
    x))

(defn deg->rad [d]
  (* 2 PI (/ d 360)))

(defn rad->deg [r]
  (* 360 (/ r (* 2 PI))))

(defn rms [vector]
  (sqrt (reduce + (map square vector))))
  
(defn euclidian [v1 v2]
  (rms (map - v2 v1)))

(defn average-angles [& as]
  (let [x (reduce + (map #(cos %) as))
        y (reduce + (map #(sin %) as))]
    (if (and (= 0 x) (= 0 y))
      nil ; note in this case there is no decernable direction
      (atan2 y x)
      )))

(defn average-angles-deg [& rads]
  (let [degs (map deg->rad rads)]
    (rad->deg (average-angles degs)))) 
        
    
