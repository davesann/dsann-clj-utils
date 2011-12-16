
(require 
  '[utils.x.core :as u]
  '[clojure.string :as s]
  '[clojure.pprint :as pp]
  '[utils.x.swingdraw :as d])

(def p pp/pprint)
(def fname "/home/dave/work/data/dictionaries/jp/edict.utf8")
(def ofname "/home/dave/work/data/dictionaries/jp/edict.cljdat")


(def edict
  (let [lines (drop 1 (u/line-seq fname))]
    (map #(s/split % #"/") lines)))

(def re-kanji-pron #"(\S+)\s*\[([^\]]+)\]\s*")

(def re2 #"\((\S+)\)\s*(.*)\s*")

(def re-pos #"^\s*\((\S+)\)\s*")
(def re-sense #"^\s*\((\d+)\)\s*")


;(p (drop 1 (re-matches re2 "(n) middle dot (typographical symbol used between parallel terms, names in katakana, etc.))")))


(defn process-def
  ([a-def] (process-def a-def []))
  ([a-def parts-of-speech]
    (if-let [sense (re-find re-sense a-def)]
      [parts-of-speech a-def]
      (if-let [[match pos] (re-find re-pos a-def)]
        (recur (subs a-def (count match)) (conj parts-of-speech (s/split pos #",")))
        [parts-of-speech a-def]))))

(defn process-line [[jp & [def1 & defs]]]
  (let [[kanji hiragana] (drop 1 (re-matches re-kanji-pron jp))
        
        [primary pron] (if kanji [kanji hiragana]
                         (let [v (s/trim jp)]
                           [v v]))
                 
        [pos d1]         (process-def def1);(drop 1 (re-matches re2 def1))
        ds (cons d1 defs)]
    {:primary primary
     :pron pron
     :pos pos
     :defs ds}
    ))


;(d/draw (map process-line  (take 10 edict)))

(let [lines edict;(take 1000 edict)
      d (map process-line lines)]
  ;d))
  (u/write-pp "/home/dave/work/data/dictionaries/jp/edict.pp.cljdat" d))


