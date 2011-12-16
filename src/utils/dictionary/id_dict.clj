(ns utils.dictionary.id-dict
  (:require 
    [clojure.string :as s]
    
    [utils.dictionary.lookup :as udl]
    [utils.seq :as useq]
    ))

(def dict-fname "/home/dave/work/data/dictionaries/id/gidic.cljdat")

(comment
(def en-id-dict
  (let [data (read-string (slurp dict-fname))] 
    (group-by first data)))

(def id-en-dict 
  (let [data (read-string (slurp dict-fname))] 
    (group-by second data)))

(def dict 
  (let [data (read-string (slurp dict-fname))] 
    (merge en-id-dict id-en-dict)))
)

(def en-id-dict
  (let [data (read-string (slurp dict-fname))]
    (udl/add {} (mapcat (fn [e] [(s/split (first e) #"\s+") e]) data))))

(def id-en-dict
  (let [data (read-string (slurp dict-fname))]
    (udl/add {} (mapcat (fn [e] [(s/split (second e) #"\s+") e]) data))))

(def dict
  (let [data (read-string (slurp dict-fname))]
    (reduce {}
      (fn [d entry]
        (-> d
          (udl/add (seq (first entry)) entry)
          (udl/add (seq (second entry)) entry)))
      data)))


(defn lookup-txt [txt]
  (map first (id-en-dict txt)))



(comment

)


