(ns utils.str
  (:require [clojure.string :as string])
  )

(defn slice 
  ([string start]
    (let [l (count string)
          offset (max 0 (if (< start 0) (+ start l) start))]
      (if (> offset l) 
        ""
        (subs string offset l))))
  ([string start end]
    (let [l (count string)
          loffset (max 0 (if (< start 0) (+ l start) start))
          roffset (min l (if (< end 0)   (+ l end)     end))]
      (if (> loffset roffset) 
        ""
        (subs string loffset roffset)
        ))))

(defn join-non-nil [sep xs]
  (string/join sep (remove nil? xs)))
