
(require 
  '[utils.x.core :as u]
  '[utils.seq :as useq]
  '[utils.map :as umap]
  '[clojure.string :as s]
  '[clojure.pprint :as pp]
  '[utils.x.swingdraw :as d]
  '[clojure.pprint :as pp]
  :reload)

(def p pp/pprint)
(def fname "/home/dave/work/data/dictionaries/cn/cedict_ts.u8")
(def ofname "/home/dave/work/data/dictionaries/cn/cedict.cljdat")
(def ofname-pp "/home/dave/work/data/dictionaries/cn/cedict.pp.cljdat")

(def re-split #"\s*(\S+)\s*(\S+)\s*\[([^\]]+)\]\s*(.*)\s*")


(def test "㶸 㶸 [xie2] /(precise meaning unknown, relates to iron)/variant of 劦 or of 協|协/")

(defn split-entry [txt]
  (let [[trad simp pinyin raw-defs] (drop 1 (re-matches re-split txt))
        all-defs (remove empty? (map s/trim (s/split raw-defs #"/")))
        [classifiers defs] (useq/partition-with #(re-matches #"CL:.*" %) all-defs)
        ]
    (umap/remove-vals empty?
      {:trad trad
       :simp simp
       :pinyin pinyin
       :defs defs
       :classifiers (map #(s/split (subs % 3) #"\|") classifiers)
       })))

(split-entry test)
    

(def cedict
  (let [lines (drop-while #(= \# (first %)) (u/line-seq fname))]
    (map split-entry lines)))

(pp/pprint (take 5 (filter :classifiers cedict)))

(u/write-prn ofname cedict)
(u/write-pp ofname-pp cedict)


