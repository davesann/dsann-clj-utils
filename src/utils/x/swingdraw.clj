(ns utils.x.swingdraw
  (:require 
    [seesaw.core :as ssc]
    [seesaw.tree :as sst]
    [clojure.pprint :as pp]
    )
  )

(defn draw [data]
  (let [txt (if (string? data) 
              data 
              (with-out-str (pp/pprint data)))]
    (ssc/invoke-later 
      (-> 
        (ssc/frame 
          :content (ssc/scrollable
                     (ssc/text 
                       :text txt 
                       :multi-line? true 
                       :editable?   false
                       :wrap-lines? true
                       )
                     :hscroll :never)
          :width 400
          :height 600
          :on-close :dispose)
        ssc/show!))))


; Make a model for the directory tree
(defn tree-model-hiccup [hiccup-data]
  (sst/simple-tree-model
    ; can open
    vector? 
    
    ;children
    (fn [[_ _ & children]] children)
    
    ;root
    hiccup-data))

(defn item-renderer-hiccup
  [renderer {:keys [value]}]
  (ssc/config! renderer 
    :text (if (vector? value)
            (let [ [t {c :class} & _ ] value] 
              (str t " : " c)) 
            (str value))))


(defn draw-tree-hiccup [hiccup-data]
  (let [tm (tree-model-hiccup hiccup-data)]
    (ssc/invoke-later 
      (-> 
        (ssc/frame 
          :content (ssc/scrollable
                     (ssc/tree :id :tree :model tm
                       :renderer item-renderer-hiccup)
                     :hscroll :never)
          :width 400
          :height 600
          :on-close :dispose)
        ssc/show!))))





;; doesn't work correctly
(comment

(defn tree-model-map [a-map]
  (sst/simple-tree-model
    ; can open
    map? 
    
    ;children
    (fn [m] (map 
              (fn [[k v]] 
                (if (map? v) v [k v]))
              m))
    
    ;root
    a-map))


(defn draw-tree-map [a-map]
  (let [tm (tree-model-map a-map)]
    (ssc/invoke-later 
      (-> 
        (ssc/frame 
          :content (ssc/scrollable
                     (ssc/tree :id :tree :model tm)
                      ; :renderer item-renderer-hiccup)
                     :hscroll :never)
          :width 400
          :height 600
          :on-close :dispose)
        ssc/show!))))


;(draw-tree-map {:a 1})
;(draw-tree-hiccup (td/prep-4-hiccup b))
)



