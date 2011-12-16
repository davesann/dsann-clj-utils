
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

(def fname-raw "/home/dave/work/data/dictionaries/jp/JMdict_e")
(def fname "/home/dave/work/data/dictionaries/jp/JMdict_e.no-entities")
(def ofname "/home/dave/work/data/dictionaries/jp/jmdict.cljdat")

(def ofname-xml "/home/dave/work/data/dictionaries/jp/jmdict.cljxml")

;; stop entity expansion into unusable strings by stripping entities
;; saved to disc -> because xml processing can use s string...
;(u/write-p fname (s/replace (slurp fname-raw) #"[&]([^;]+);" "$1"))

  
;(def jmdict (lxml/parse-seq fname))
(def jmdict (lxml/parse-trim fname))


;;; process the xml
(defn merge-senses [senses]
  (map umap/concat-vals 
    (map umap/merge-as-list senses))) 
  
(defn process-senses
  ([senses] 
    (process-senses (merge-senses senses) nil []))
  ([[s & senses] pos result]
    (if (nil? s)
      (umap/mapvals #(map :gloss %) (group-by :pos result))
      (let [pos (if-let [p (:pos s)]
                  (map keyword p)
                  pos)]
        (recur senses pos (conj result (assoc s :pos pos)))))))
  
  
(defn process-entry [c]
  (let [e (group-by first c)
        senses (map second (:sense e))
        re     (:r_ele e)
        ke     (:k_ele e)
        ]
    (umap/remove-vals empty?
      { 
       :senses (process-senses senses);(map-indexed (fn [i x] [i x]) senses)
       :r (apply concat (map second re))
       :k (apply concat (map second ke))
       })))

(defn walk-f [e]
  (if-let [t (:tag e)]
    (cond  
      (useq/in [:reb :keb] t)
      (useq/unit-seq->unit (remove nil? (:content e)))
      
      (useq/in [:sense 
                :k_ele 
                :r_ele 
                :gloss 
                :pos 
                :misc
                ] t)
      [t (remove nil? (:content e))]
      
      (= :entry t)
      (process-entry (remove nil? (:content e)))
      
      true
      nil
      ;[(keyword (str "def-" (name t))) (remove nil? (:content e))]
      )
    e))

(def dict (w/postwalk walk-f (:content jmdict)))

;; end process the XML

;; write to output file
;(u/write-pp ofname dict)






;; HERE be tests and repls
(def dict2 (read-string (slurp ofname)))

(= dict dict2)


(d/draw (w/postwalk walk-f (take 200 (:content jmdict))))


(p (w/postwalk walk-f (take 1 (:content jmdict))))



(p (w/postwalk walk-f (take 10 (:content jmdict))))

(p (w/postwalk walk-f (nth (:content jmdict) 100)))

(p (take 10 (:content jmdict)))



(def seq-of-maps  [{:a [1 2]} {:a [1 4]} {:a [2 5]}])

(apply merge-as-list seq-of-maps)


(def example-senses
  [ 
   [[:pos ["int"]]
    [:gloss ["yes"]]
    [:gloss ["that is correct"]]
    [:gloss ["right"]]]
   [[:gloss ["um"]] [:gloss ["errr"]]]
   [[:gloss ["huh?"]]]
   [[:gloss ["grrr"]] [:gloss ["gah"]] [:gloss ["Must I?"]]]
   [[:pos ["adj-f"]] [:gloss ["good"]]]]
  )


;(recur senses pos (conj result r))))))
      
(p (process-senses example-senses))

(p (umap/mapvals (fn [v] (map #(dissoc % :pos) v)) (group-by :pos (process-senses example-senses))))


(p (umap/mapvals #(map :gloss %) (group-by :pos (process-senses example-senses))))

(p (map merge-as-list example-senses))

(p (map (fn [m] (umap/mapvals #(apply concat %) m)) (map merge-as-list example-senses)))

    




(defn xmltree->cljdat [xml-tree]
    (cond
      (map? xml-tree)
      (let [{:keys [tag attrs content]} xml-tree]
        {tag 
         (case tag
           :entry 
           (apply (partial merge-with conj) (xmltree->cljdat content))
           
           :re_ele
           (first content)
           
           :reb
           content
           
           (map xmltree->cljdat content)
           
           )
         })
      
      (sequential? xml-tree)
      (map xmltree->cljdat xml-tree)
    
      true
      xml-tree))
  
(p (take 10 (xmltree->cljdat (:content jmdict))))



(defn multi-content
  ([xml-tree] (multi-content xml-tree #{}))
  ([xml-tree result]
    (cond
      (map? xml-tree)
      (let [{:keys [tag content]} xml-tree]
        (if (> (count content) 1)
          (recur content (conj result tag))
          (recur content result)))
      
      (sequential? xml-tree)
      (let [sets (map multi-content xml-tree)]
        (apply cset/union (cons result sets)))
      
      true
      result)))
  
(p (multi-content (take 10000 (:content jmdict))))

(def x (doall (take 1000 (xmltree->cljdat (:content jmdict)))))

(p (multi-content jmdict))

(p (take 10 (xmltree->cljdat (:content jmdict))))





(u/write-pp ofname2 (take 1000 (:content jmdict)))

(u/write-pp ofname (take 1000 (xmltree->cljdat (:content jmdict))))


