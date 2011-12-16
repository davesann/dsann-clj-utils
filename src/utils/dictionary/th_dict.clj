(ns utils.dictionary.th-dict
  (:require 
    [utils.dictionary.lookup :as udl]
    [utils.unicode :as uu]
    ))

(def dict
  (let [data (read-string (slurp  "/home/dave/work/data/dictionaries/th/telex.2.cljdat"))]
    (reduce (fn [d entry]
              (udl/add d [(uu/grapheme-split (:tsearch entry)) entry]))
      {}
      data)))
