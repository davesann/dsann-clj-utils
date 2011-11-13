(ns utils.dictionary.jp-dict
  (:require [utils.dictionary.lookup :as udl]
    )
  )

(def dict (udl/load-jp-dict))