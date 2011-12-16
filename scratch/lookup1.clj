
(set! *print-length* 100)
(set! *print-level* 10)

(require '[clojure.pprint :as pp])
(require '[net.cgrand.enlive-html :as enlive])
(require '[clojure.xml :as xml])
(require '[clojure.contrib.lazy-xml :as lxml])
(require '[clojure.zip :as zip])
(require '[clojure.contrib.zip-filter.xml :as zf])
(require '[clojure.string :as s])
(require '[utils.x.core :as u] :reload)
(require '[utils.str :as ustr])
(require '[utils.seq :as useq])


(defn make-entries [entry-lists]
  (map (fn [l]
         (apply merge
           (map (fn [[start-e chars-e end-e]] 
                  {(:name start-e) (:str chars-e)})
             l)))
    entry-lists))
    

(defn extract-entries [parse-seq]
  (let [f #(not (and 
                  (= (:type %) :start-element)
                  (= (:name %) :entry)))
        ]
      (make-entries
        (remove empty?
          (map #(partition 3 %) 
            (partition-by f (drop-while f parse-seq)))))
      ))

;(pp/pprint
(let [ofile   "/home/dave/work/data/dictionaries/th/telex.2.cljdat"
      xml-uri "file:///home/dave/work/data/dictionaries/th/lexitron_2.0_xml/telex.xml"
      ]
  (u/write-prn ofile  (extract-entries (lxml/parse-seq xml-uri))))
;)
  

(let [data (read-string (slurp  "/home/dave/work/data/dictionaries/th/telex.2.cljdat"))]
  (pp/pprint (take 10 data)))


(let [ofile   "/home/dave/work/data/dictionaries/th/telex.cljdat"
      xml-uri "file:///home/dave/work/data/dictionaries/th/lexitron_2.0_xml/telex.xml"
      xml-data (enlive/xml-resource (java.net.URL. xml-uri))]
  (u/log "here")
  (u/write-pp
    ofile
    (enlive/select xml-data [:entry])
    ))


(def words-xml-uri "file:///dataset/work/data/dictionaries/XWN2.0-1.1/adv.xml")



;(defn fetch-url [url]
;  (enlive/html-resource (java.net.URL. url)))

(def xml-data (enlive/xml-resource (java.net.URL. words-xml-uri)))


;(split-with
;                      #(= \" (first (u/log %)))


(take 1 (enlive/select xml-data [:xwn :gloss]))

(defn extract-definition-from-text [text]
  (let [splits (s/split 
                 (s/trim (first (:content text)))
                 #"\s*;\s*")
        [defs usages] (split-with  #(not (= \" (first %))) splits)
        ]
   {:defs defs
    :usage (map #(ustr/slice 1 -1 %) usages)
    }))
                        
(defn extract-synsets-from-text [text]
  {:synset (map #(s/replace % "_" " ") 
             (s/split
               (s/trim (first (:content text)))
               #"\s*,\s*"))
   })


(defn extract-definitions [xml-data]
  (let [glosses (enlive/select xml-data [:xwn :gloss])
        attr-seq   (map :attrs glosses)
        synset-seq (map extract-synsets-from-text
                     (enlive/select glosses [:synonymSet]))
        text-seq   (map extract-definition-from-text
                     (enlive/select glosses [:text]))
        ]
    (map merge
      attr-seq
      synset-seq
      text-seq)
    )
  )

(pp/pprint
(take 100 (filter #(useq/in (:synset %) "bad" ) (extract-definitions xml-data))
  ))

