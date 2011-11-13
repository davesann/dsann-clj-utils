(ns utils.color.str
  (:require 
    [clojure.string :as string]    
    [utils.color.protocols :as cp]
  )
)

(defn b-join [tag items]
  (str tag "(" (string/join "," items) ")"))

(defn css-rgb-str 
  ([r]       (css-rgb-str r r r ))
  ([r g b]   (b-join "rgb" [r g b]))
  ([r g b a] (b-join "rgba" [r g b a])))

(defn ->css-rgba [color]
  (let [[r g b a] (cp/rgba color)]
    (css-rgb-str r g b a)))

(defn ->css-rgb [color]
  (let [[r g b] (cp/rgb color)]
    (css-rgb-str r g b)))

(defn ->rgb-hex [color]
  (let [[r g b] (cp/rgb color)] 
    (format "#%2X%2X%2X" r g b)))
