
(require 
  '[utils.x.core :as u]
  '[utils.seq :as useq]
  '[utils.map :as umap]
  '[clojure.string :as s]
  '[clojure.pprint :as pp]
  '[utils.x.swingdraw :as d]
  '[clojure.xml :as xml]
  '[clojure.set :as cset]
  '[clojure.walk :as w]
  '[clojure.contrib.lazy-xml :as lxml]
  )

(def p pp/pprint)

(set! *print-length* 100)
(set! *print-level* 20)

(def fname "/home/dave/work/data/dictionaries/id/gidic.txt")
(def ofname "/home/dave/work/data/dictionaries/id/gidic.cljdat")
(def en-id (map #(s/split % #"\s*[|]\s*") 
             (s/split (slurp fname) #"\s*\n+\s*" )))


(defn xor-n 
  ([n & xs] (= nil (first (drop n (filter #(= true %) xs))))))
    

(defn xor 
  ([pred? & xs] 
    (let [[f r] (filter true? (map pred? xs))]
      (and 
        (= true f) 
        (nil? r)))))
    
    
  


(defn process-entry [[en id]]
  (let [c1 (re-find #";" en)
        c2 (re-find #";" id)]
    (cond 
      (and (nil? c1) (nil? c2))
      [[en id]]
      
      (nil? c2)
      (for [s (s/split en #"\s*;\s*")]
        [s id])
      
      (nil? c1)
      (for [s (s/split id #"\s*;\s*")]
        [en s])
      
      true
      [[(s/replace en #"\s*;\s*" "/") (s/replace id #"\s*;\s*" "/")]]
      )))
      

(p (apply concat (map process-entry en-id)))


(u/write-pp ofname (apply concat  (map process-entry en-id)))


