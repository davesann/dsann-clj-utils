(ns utils.x.annotations
  (:require 
    [clojure.string :as s]
    [clojure.contrib.str-utils2 :as ccsu]
    [utils.annotation-fns :as ua]
    [utils.map :as umap]
    )
  )


(defn partition-re [re match-tag non-match-tag txt]
  (let [p (ccsu/partition txt re)]
    (if (empty? (first p))
      (map (fn [tag v] {:tag tag :match v}) 
           (cycle [match-tag non-match-tag])
           (drop 1 p))
      (map (fn [tag v] {:tag tag :match v}) 
           (cycle [non-match-tag match-tag])
           p))))

;; can move these => generic
;;; except these REs are not supported by javascript
(def word-split-spaced-re    #"(?:\p{C}|\p{S}|\p{Z}|(?:\p{P}\p{Z}))+")
(def word-split-unspaced-re  #"(?:\p{P}|\p{Z}|\p{S}|\p{N})+")    


(defn split-text-spaced [txt]
  (s/split txt word-split-spaced-re))

(defn split-text-unspaced [txt]
  (map s/trim (s/split txt word-split-unspaced-re)))

(defn partition-txt-spaced [txt]
  (partition-re word-split-spaced-re :space :word txt))

(defn partition-txt-unspaced [txt]
  (partition-re word-split-unspaced-re :space :word txt))

(defn space-annotate [txt]
  (apply merge
         (ua/partition->annotations count (partition-txt-spaced txt))))

(defn space-annotation [annotation-data]
  (= :space (:tag annotation-data))) 

(defn lookup [dict annotation-map]
  (umap/mapvals 
    #(if (not (space-annotation %))
       (assoc % :lookup (dict (:match %) "??"))
       %)
    annotation-map))
  


