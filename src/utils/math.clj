(ns utils.math)

;(load "x/math")

(defn abs [n]
  (if (neg? n) (- n) n))

(defn average 
  "Finds the average of the numbers entered"
  [& all]
  (/ (apply + all) (count all)))

(defn clamp [min-x max-x x]
  (cond 
    (< x min-x)
    min-x
    
    (> x max-x)
    max-x
    
    true
    x))

(defn cos [n]
  (java.lang.Math/cos (double n)))
  
(defn sin [n]
  (java.lang.Math/sin (double n)))
  
(defn atan2 [x y]
  (java.lang.Math/atan2 (double x) (double y)))

(def PI java.lang.Math/PI)

(defn deg->rad [d]
  (* 2 PI (/ d 360)))

(defn rad->deg [r]
  (* 360 (/ r (* 2 PI))))
          
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
        
    
