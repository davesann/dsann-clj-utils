(ns utils.color.protocol-utils
  (:require 
    [utils.map :as um]
    [utils.x.core :as u]
  )
)

;; conversion utility functions
(defn convert-color [color vals-fn convert-fn make-fn]
  (let [vals (vals-fn color)
        c-vals (convert-fn vals)]
    (apply make-fn (concat c-vals [(:alpha color)])))) 

(defn convert-rgb [rgb-color convert-fn make-fn]
  (convert-color
    rgb-color
    (fn [color] (um/select-vals color :red :green :blue))
    convert-fn
    make-fn))

(defn convert-hsl [hsl-color convert-fn make-fn]
  (convert-color
    hsl-color
    (fn [color] (um/select-vals color :hue :saturation :lightness))
    convert-fn
    make-fn))

(defn convert-cmyk [cmyk-color convert-fn make-fn]
  (convert-color
    cmyk-color
    (fn [color] (um/select-vals color :cyan :magenta :yellow :k))
    convert-fn
    make-fn))

(defn convert-cmy [cmy-color convert-fn make-fn]
  (convert-color
    cmy-color
    (fn [color] (um/select-vals color :cyan :magenta :yellow))
    convert-fn
    make-fn))
