(ns utils.dictionary.cn-dict
  (:require [utils.dictionary.lookup :as udl]
    )
  )

(def dict (udl/load-cn-dict))