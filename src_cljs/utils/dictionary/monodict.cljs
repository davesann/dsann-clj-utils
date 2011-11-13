(ns utils.dictionary.monodict
  (:require 
    [utils.x.core :as u]
    ))

(defn make-dict [] {})

(defn add-word [d word trans]
  (let [d (or d {})
        mbt (:map-by-trans (d word))
        c (get mbt trans 0)
        d1 (assoc-in d [word :map-by-trans trans] (inc c))
        new-mbt (:map-by-trans (d1 word))
        d2 (assoc-in d1 [word :list-by-count] (sort-by second (u/comparator >) (seq new-mbt)))
        ]
    d2))

(defn add-words [d [word trans & others]]
  (let [new-d (add-word d word trans)]
    (if (nil? others)
      new-d
      (recur new-d others))))
  

(defn lookup-by-freq [d word]
  (:list-by-count (get d word)))

(defn lookup-first [d word]
  (first 
    (first (lookup-by-freq d word))))
