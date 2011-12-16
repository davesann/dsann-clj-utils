(ns utils.dictionary.jp-dict
  (:require 
    [clojure.set :as cset]
    [utils.dictionary.lookup :as udl]
    [utils.dictionary.jp-inflect :as jpi]
    [utils.x.core :as u]
    [utils.seq :as useq]
    [utils.map :as umap]
    [utils.str :as ustr]
    [utils.ctrl :as uctrl]
    
    )
  )

(defn entry-pos [e]
  (apply concat (keys (:senses e))))

(defn has-pos [pos-set entry]
  (if (some pos-set (entry-pos entry))
    entry))

(defn multi-pos [pos-set entry]
  (if (> (count (cset/intersection pos-set (set (entry-pos entry)))) 1)
    entry))

(defn stemify [entry]
  (if-let [stem (some jpi/stems-kana (entry-pos entry))]
    (assoc entry :stems 
      (map #(ustr/remove-ending stem %) 
        (concat (:k entry) (:r entry))))
    entry))
 
(defn make-lookup-dict [dict]
  (reduce 
    (fn [d entry]
      (let [rs (map seq (:r entry))
            ks (map seq (:k entry))
            ]
        (-> d 
          (udl/add (interleave rs (repeat entry)))
          (udl/add (interleave ks (repeat entry))))))
    {}
    dict))

(defn make-stems-lookup-dict [dict]
  (reduce 
    (fn [d entry]
      (let [stems (:stems entry)]
        (udl/add d (interleave stems (repeat entry)))))
    {}
    dict))

(defn stem? [l]
  (or (:stems l) false))


(defn find-inflection-candidates [stxt lookups-stems]
  (apply merge 
    (useq/map- nil?  
      #(let [v (jpi/lookup-and-group-by-pos 
                 (drop (count %) stxt))]
         (if (empty? v) nil {% v}))
      (keys lookups-stems))))


(defn inflection-senses [senses inflection-candidates]
  (useq/mapcat- empty?
    (fn [[poses senses]]
      (useq/map- nil?
        (fn [pos]
          (if-let [inflections (inflection-candidates pos)]
            [pos inflections senses])) 
        poses))
    senses
    ))
           
(defn apply-inflection-candidates [stem-lookups inflection-candidates]
  (if inflection-candidates
    (apply umap/merge-as-list 
      (useq/mapcat- nil? 
        (fn [[stem entries]]
          (if-let [v (useq/mapcat- nil? 
                       (fn [e] 
                         (uctrl/if-let 
                           [stem-ics   (inflection-candidates stem)
                            new-senses (inflection-senses (:senses e) stem-ics)]
                           (remove nil?
                             (for 
                               [[pos inflections senses] new-senses
                                [pos2 desc inflection]   inflections]
                               {(concat stem inflection) 
                                (assoc e :senses {pos senses} 
                                  :inflection inflection
                                  :inflection-desc desc)}
                               ))))
                       entries)]
            v))
        stem-lookups))))


(def fname "/home/dave/work/data/dictionaries/jp/jmdict.cljdat")
(def dict (read-string (slurp fname)))
(def stems-dict (useq/map- #(nil? (:stems %)) stemify dict))
(def words-dict (useq/map- #(:stems %) stemify dict))
(def stems-lookup-dict (make-stems-lookup-dict stems-dict))
(def words-lookup-dict (make-lookup-dict words-dict))

(defn lookup-txt [txt]
  (let [stxt    (seq txt)
        lookups-words (udl/lookup words-lookup-dict stxt)
        lookups-stems (udl/lookup stems-lookup-dict stxt)
       
        inflection-candidates 
        (find-inflection-candidates stxt lookups-stems)
        
        lookups-inflections
        (apply-inflection-candidates lookups-stems inflection-candidates)
        ]
    (merge-with concat lookups-words lookups-inflections)))


(comment
  
  (def txt (jpt/roma->kana-str "arimasendeshita"))
  (def ls  (udl/lookup jp/stems-lookup-dict txt))
  (def ics (jp/find-inflection-candidates txt ls))
    
  
(require '[utils.transliterate.jp :as jpt] :reload)
(require '[utils.unicode.jp :as ujp] :reload)

(require '[utils.dictionary.jp :as jp] :reload)
(require '[utils.dictionary.jp-inflect :as jpi] :reload)
(require '[utils.dictionary.lookup :as udl] :reload)

(require '[clojure.pprint :as pp])
(require '[clojure.string :as s])
(require '[utils.x.core :as u])
(require '[utils.map :as umap]  :reload)
(require '[utils.seq :as useq]  :reload)
(require '[utils.ctrl :as uctrl] :reload)
(require '[utils.x.swingdraw :as d]) 


(def p pp/pprint)

(def txt "耽りながら")
(def txt "耽ru")
(d/draw (jp/lookup-txt (jpt/roma->kana  txt)))

(d/draw (jp/lookup-txt (jpt/roma->kana "出かけmashita")))
(d/draw (lookup-txt (jpt/roma->kana "yonda")))

  
(d/draw (first jpi/inflections-lookup-kana))
;(take 5 (remove nil? (map #(multi-pos poses %) dict)))




(def test-stem-inflection-candidates
  {[\出 \か]    {:v5k [[:v5k "imperative" "け"]]}
   [\出 \か \け] {:v1  [[:v1 "past" "ました"]]}}
  )


(def test-inflection-candidates
  {:v1  [[:v1 "past" "ました"]]}
  )

(def test-inflection-senses
  {[:v1 :vi]
   [["to depart"
     "to go out (e.g. on an excursion or outing)"
     "to set out"
     "to start"
     "to be going out"]]}
  )


(def test-inflection-lookups
  {[\出 \か \け]
   [{:stem true,
     :senses
     {[:v1 :vi]
      [["to depart"
        "to go out (e.g. on an excursion or outing)"
        "to set out"
        "to start"
        "to be going out"]]}
     :r ["でかけ"],
     :k ["出掛け" "出かけ"]}
    ]}
  )

(def yonda-lookups-stems
   {[\読]
    (list {:stem true,
      :senses {(list :v5m :vt) (list (list "to read"))},
      :r (list "よ"),
      :k (list "読")})}
   )

 (find-inflection-candidates (seq (jpt/roma->kana "読nda")) yonda-lookups-stems)


(p (inflection-senses test-inflection-senses test-inflection-candidates))
     
(p (apply-inflection-candidates test-inflection-lookups test-stem-inflection-candidates))





)
