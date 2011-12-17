(ns dsann.utils.str
  (:require [clojure.string :as s])
  )

(defn slice 
  ([start string]
    (let [l (count string)
          offset (max 0 (if (< start 0) (+ start l) start))]
      (if (> offset l) 
        ""
        (subs string offset l))))
  ([start end string]
    (let [l (count string)
          loffset (max 0 (if (< start 0) (+ l start) start))
          roffset (min l (if (< end 0)   (+ l end)     end))]
      (if (> loffset roffset) 
        ""
        (subs string loffset roffset)
        ))))

(defn join-non-nil [sep xs]
  (s/join sep (remove nil? xs)))

(defn begins-with [beginning string]
  (let [l (count beginning)]
    (= beginning (subs string 0 l))))

(defn ends-with [ending string]
  (let [l (- (count ending))]
    (= ending (slice l string))))

(defn remove-ending [ending string]
  (let [l (- (count ending))]
    (if (= ending (slice l string))
      (slice 0 l string))))
           
(defn remove-beginning [beginning string]
  (let [l (count beginning)]
    (if (= beginning (subs string 0 l))
      (subs string l))))

(defn whitespace? [string]
  (empty? (s/trim string)))