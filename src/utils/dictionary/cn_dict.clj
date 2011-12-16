(ns utils.dictionary.cn-dict
  (:require
    [clojure.string :as s]
    [utils.dictionary.lookup :as udl]
    )
  )

(def dict
  (let [data (read-string (slurp "/home/dave/work/data/dictionaries/cn/cedict.cljdat"))]
    (reduce (fn [d entry]
              (let [t (:trad entry)
                    s (:simp entry)
                    add-if (fn [d] (if (not (= s t))
                                     (udl/add d [(seq s) entry])
                                     d)) 
                    ]
                (-> d
                  (udl/add [(seq t) entry])
                  (add-if)
                  (udl/add [(s/split (:pinyin entry) #"\s+") entry])
                  )))
      {}
      data)))
                  
