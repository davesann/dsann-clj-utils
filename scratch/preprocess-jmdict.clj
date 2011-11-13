
(require 
  '[utils.x.core :as u]
  '[clojure.string :as s]
  '[clojure.pprint :as pp]
  '[utils.x.swingdraw :as d])

(def p pp/pprint)
(def fname "/home/dave/work/data/dictionaries/cn/cedict_ts.u8")
(def ofname "/home/dave/work/data/dictionaries/cn/cedict.cljdat")
(def ofname-pp "/home/dave/work/data/dictionaries/cn/cedict.pp.cljdat")

(def re-split #"\s*(\S+)\s*(\S+)\s*\[([^\]]+)\]\s*(.*)\s*")


(def test "㶸 㶸 [xie2] /(precise meaning unknown, relates to iron)/variant of 劦 or of 協|协/")

(defn split-entry [txt]
  (let [[trad simp pinyin raw-defs] (drop 1 (re-matches re-split txt))
        defs (remove empty? (map s/trim (s/split raw-defs #"/")))
        ]
    {:trad trad
     :simp simp
     :pinyin pinyin
     :defs defs}))

(split-entry test)
    

(def cedict
  (let [lines (drop-while #(= \# (first %)) (u/line-seq fname))]
    (map split-entry lines)))


(u/write-prn ofname cedict)
(u/write-pp ofname-pp cedict)


