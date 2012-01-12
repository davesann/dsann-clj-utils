(ns dsann.utils.x.core)



(defn map->js [m]
  (let [out (js-obj)]
    (doseq [[k v] m]
      (aset out (name k) v))
    out))


(defn log 
  ([x]
    (let [l (if (map? x)
              (map->js x)
              x)]
      (.log js/console l)
      x))
  ([m x]
    (do 
      (log {:msg m :data x})
      x)))

(defn log-str 
  ([x]
    (do
      (log (pr-str x))
      x))
  ([m x]
    (do 
      (log-str {:msg m :data x})
      x)))

(defn comparator [x]
  (fn->comparator x))

