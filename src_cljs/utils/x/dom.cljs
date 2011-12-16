(ns utils.x.dom
  (:require  
    [pinot.events :as pe]
    [pinot.html :as ph]
    [pinot.dom :as pdom]
    [goog.dom :as gdom]
    [utils.x.core :as u]
    ))


(defn query-selector-all-js [selector element]
  (let [element (or element js/document)]
    (.querySelectorAll element selector)))

(defn query-selector-all [selector element]
  (if-let [r (query-selector-all-js selector element)]
    (pdom/nodelist->coll r)))

(defn get-element-by-class [class element]
  (gdom/getElementByClass class element))

(defn get-elements-by-class 
  ([class element]
    (pdom/nodelist->coll
      (gdom/getElementsByClass class element))))

(defn body []
  (.body js/document))

