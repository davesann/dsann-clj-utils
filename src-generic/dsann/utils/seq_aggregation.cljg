(ns dsann.utils.seq-aggregation)

;; Alpha

;; count and sum
(defn reduce-if [op pred? identity-value value-fn colls]
  (reduce op 
          (map #(if (pred? %) (value-fn %) identity-value) 
               (apply concat colls))))

(defn sum-if [pred? value-fn & colls]
  (reduce-if + pred? 0 value-fn colls))

(defn count-if [pred? & colls]
  (reduce-if + pred? 0 (fn [_] 1) colls))

(defn reduce-map [op f colls]
  (reduce op (map f (apply concat colls))))

(defn prod-f [f & colls]
  (reduce-map * f colls))

(defn sum-f [f & colls]
  (reduce-map + f colls))
