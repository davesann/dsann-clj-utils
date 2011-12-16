(ns utils.x.dom.node
  (:require
    [pinot.dom :as pdom]
    [goog.dom :as gdom]
    [goog.style :as gstyle]

    [utils.x.dom :as udom]
    [utils.x.core :as u]
    [utils.seq :as useq]
    [utils.str :as ustr]
    [utils.html :as uh]
    )
  ) 



(defn height [n]
  (.height (gstyle/getSize n)))

(defn width [n]
  (.width (gstyle/getSize n)))


(defn set-padding! [n p edge]
  (let [attr (join-non-nil "-" ["padding" (name edge)])]
    (gstyle/setStyle n attr p)))


(defn set-margin! [n p edge]
  (let [attr (ustr/join-non-nil "-" ["margin" (name edge)])]
    (gstyle/setStyle n attr p)))

(defn parent [n]
  (.parentNode n))

;; TEXT

(def TEXT-NODE 3)

(defn text-node? [n]
  (= TEXT_NODE (.nodeType n)))

(defn text-nodes [node]
  (gdom/findNodes node text-node?))

(defn split-text-node [n split-fn node-fn]
  (let [strings (split-fn (gdom/getTextContent n))
        new-node (first (ph/html [:span (map node-fn strings)]))
        ]
    (gdom/replaceNode new-node n)
    (gdom/flattenElement new-node)
    ))

(defn replace-content [parent children]
  (gdom/removeChildren parent)
  (if (seq? children)
    (doseq [c children]
      (gdom/append parent c))
    (gdom/append parent children)))

(defn has-children? [node]
  (> (.length (gdom/getChildren node)) 0))



(defn get-text [node]
  (gdom/getTextContent node))



;; Create and use oriented functions 
;; create specific functions that orient height and width
;; height of a left component in the dom width
;; height of a top  component in the dom height
;; etc.
;; useful for generic resizing functions where the component 
;; may be top, botton, left or right.

(defn top? [location]
  (= :top location))

(defn bottom? [location]
  (= :bottom location))

(defn left? [location]
  (= :left location))

(defn right? [location]
  (= :right location))

(defn top-bottom? [o]
  (or (top? o) (bottom? o)))

(defn left-right? [o]
  (or (left? o) (right? o)))

(defn height-o [location]
  (if (left-right? location)
    (fn [n] (width n))
    (fn [n] (height n))
    ))

(defn width-o [location]
  (if (left-right? location)
    (fn [n] (height n))
    (fn [n] (width n))
    ))

(defn set-height-o! [location]
  (if (left-right? location)
    (fn [n h] (gstyle/setWidth n h))
    (fn [n h] (gstyle/setHeight n h))))
  
(defn client-y-o [location]
  (if (left-right? location)
    (fn [event] (.clientX event))
    (fn [event] (.clientY event))))

(defn set-padding-top-o [location]
  (fn [n p] (set-padding! n p location)))
    
(defn resize-height-anim-o [orientation]
  (if (left-right? orientation)
    (fn [n h1 h2 t]
      (gfxdom/ResizeWidth. n h1 h2 t))
    (fn [n h1 h2 t]
      (gfxdom/ResizeHeight. n h1 h2 t))
    ))

