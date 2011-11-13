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

(defn pow [x n]
  (.pow js/Math x n))

(defn round [x n]
  (let [m (pow 10 n)] 
    (/ (.round js/Math (* m x)) m)))


(defn length [string]
  (.length string))

(defn match [string re]
  (.match string re))

(defn search [string re]
  (.search string re))

(defn pow [x n]
  (.pow js/Math x n))

(defn round [x n]
  (let [m (pow 10 n)] 
    (/ (.round js/Math (* m x)) m)))

(defn re-pattern-m [re modifiers]
  (js/RegExp. re modifiers))

(defn comparator [x]
  (fn->comparator x))


(defn ^:export ->keyword [txt]
  (if (= (nth txt 0) \:)
    (keyword (drop 1 txt))
    txt))


(defn ^:export to-keyword [s]
  (keyword s))
  