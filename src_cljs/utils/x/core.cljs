(ns utils.x.core)

(defn make-js-map
  "makes a javascript map from a clojure one"
  [cljmap]
  (let [out (js-obj)]
    (doall (map #(aset out
                       (name (first %))
                       (if (map? (second %))
                         (make-js-map (second %))
                         (second %)))
                cljmap))
    ;;(dbg (str "js map: " (js* "JSON.stringify(~{})" out)))
    out))


(defn log [x]
  (let [l (if (map? x)
            (make-js-map x)
            x)]
    (.log js/console l)
    x))

(defn logm [m x]
  (do 
    (log {:msg m :data x})
    x))


(defn log-str [x]
  (.log js/console (print-str x))
  x)


(defn logm-str [m x]
  (do 
    (log-str {:msg m :data x})
    x))


(defn comparator [x]
  (fn->comparator x))

