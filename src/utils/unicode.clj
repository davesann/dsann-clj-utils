(ns utils.unicode
  (:require 
    
    [clojure.string :as string]
    
    [utils.x.core :as u]
    [utils.x.parse-numbers :as upn]
    
    [utils.seq :as useq]
    [utils.map :as umap]
    )
  )

(def unicode-char-fields
  [
   ;:char
   ;:code-int
   
   :code-str
   :character-name	
   :general-category
   :canonical-combining-classes
   :bidirectional-category
   :character-decomposition-mapping
   :decimal-digit-value
   :digit-value	
   :numeric-value
   :mirrored
   :unicode-1-name
   :comment-field-10646
   :uppercase-mapping
   :lowercase-mapping
   :titlecase-mapping])

;(def LOCAL-UNICODE-DATA "resources/utils/UnicodeData.txt")
(def LOCAL-UNICODE-DATA "/dataset/work/dev/non-IDE/clojure/utils/resources/utils/UnicodeData.txt")
(def LOCAL-UNICODE-BLOCKS "resources/utils/Blocks.txt")

  
(defn make-char-def [str-fields]
  (let [code-int (upn/parse-int (first str-fields) 16)
        ]
    (assoc (zipmap unicode-char-fields str-fields)
           :code-int code-int)))
  
(defn load-unicode-data
  ([] (load-unicode-data LOCAL-UNICODE-DATA))
  ([unicode-data-file]
    (let [lines (u/line-seq unicode-data-file)
          splits (map #(string/split % #";") lines)
          all-defs (map make-char-def splits)
          chardefs-16 (take-while #(<= (:code-int %) 0xFFFF) all-defs) 
          ]
      chardefs-16    
      )))

(defn load-blocks 
  ([] (load-blocks LOCAL-UNICODE-BLOCKS))
  ([unicode-block-file]
    (let [lines (remove
                  #(or (empty? %) (= \# (first %)))
                  (map string/trim 
                    (u/line-seq unicode-block-file)))
          all-defs (map #(let [[codes name] (map string/trim (string/split % #";"))
                               [from to] (string/split codes #"[.][.]")
                               ]
                           {:from (upn/str16->int from)
                            :to (upn/str16->int to)
                            :block name})
                     lines)
          blocks-16 (take-while #(<= (:to %) 0xFFFF) all-defs) 
          ]
      blocks-16    
      )))

(defn unicode-data-map 
  ([] (unicode-data-map (load-unicode-data)))
  ([unicode-data]
    (into {} (map (fn [x] [(:code-int x) x])
               unicode-data))))
    
(defn categories [ucd]
  (let [gc (:general-category ucd)]
    (map #(keyword (str %)) gc)))
    
(defn major-category [ucd]
  (first (categories ucd)))

(defn minor-category [ucd]
  (second (categories ucd)))

(defn letter? [ucd]
  (= :L (major-category ucd)))

(defn major-groups [unicode]
  (group-by major-category unicode))

(defn unicode-seqs [unicode-defs]
  (let [ps (useq/partition-sequential inc (map :code-int unicode-defs))]
    (apply str
           (map 
             #(if (sequential? %) 
                (string/join "-" (map char %))
                (char %))
             ps
             )))
  )

(defn any-p? [[p & preds] v]
  (if (nil? p) false
    (if (p v)
      true
      (recur preds v))))


(def unicode-map (unicode-data-map))

(def CR   \u000D)
(def LF   \u000A)
(def ZWNJ \u200C)
(def ZWJ  \u200D)

(defn cr? [code-point]
  (= CR code-point))

(defn lf? [code-point]
  (= LF code-point)) 

(defn zwnj? [code-point]
  (= ZWNJ code-point))

(defn zwj? [code-point]
  (= ZWJ code-point))

(defn control-char? [code-point]
  (if-let [u (unicode-map (int code-point))]
    (and 
      (some #{(:general-category code-point)} ["Zl" "Zp" "Cc" "Cf"])
      (not (any-p? [cr? lf? zwnj? zwj?] code-point)))))
                            
(defn extending-char? [code-point]
  (if-let [u (unicode-map (int code-point))]
    (let [gc (:general-category u)]
      (or (= "Mn" gc) (= "Me" gc)
        (any-p? [zwnj? zwj?] code-point)))))
      
(defn spacing-mark? [code-point]
  (if-let [u (unicode-map (int code-point))]
    (or (= (:general-category u) "Mc") ; Spacing combining mark
      (some #{code-point}
        [
         \u0E30 ;( ะ ) THAI CHARACTER SARA A
         \u0E32 ;( า ) THAI CHARACTER SARA AA
         \u0E33 ;( ำ ) THAI CHARACTER SARA AM
         \u0E45 ;( ๅ ) THAI CHARACTER LAKKHANGYAO
         \u0EB0 ;( ະ ) LAO VOWEL SIGN A
         \u0EB2 ;( າ ) LAO VOWEL SIGN AA
         \u0EB3 ;( ຳ ) LAO VOWEL SIGN AM
         ;and Grapheme_Cluster_Break ≠ Extend  ??? WHAT IS THIS?
         ]))))

(defn prepending-char? [code-point]
  (some #{code-point}
    [
     \u0E40 ;( เ ) THAI CHARACTER SARA E
     \u0E41 ;( แ ) THAI CHARACTER SARA AE
     \u0E42 ;( โ ) THAI CHARACTER SARA O
     \u0E43 ;( ใ ) THAI CHARACTER SARA AI MAIMUAN
     \u0E44 ;( ไ ) THAI CHARACTER SARA AI MAIMALAI
     \u0EC0 ;( ເ ) LAO VOWEL SIGN E
     \u0EC1 ;( ແ ) LAO VOWEL SIGN EI
     \u0EC2 ;( ໂ ) LAO VOWEL SIGN O
     \u0EC3 ;( ໃ ) LAO VOWEL SIGN AY
     \u0EC4 ;( ໄ ) LAO VOWEL SIGN AI
     \uAAB5 ;( ꪵ ) TAI VIET VOWEL E
     \uAAB6 ;( ꪶ ) TAI VIET VOWEL O
     \uAAB9 ;( ꪹ ) TAI VIET VOWEL UEA
     \uAABB ;( ꪻ ) TAI VIET VOWEL AUE
     \uAABC ;( ꪼ ) TAI VIET VOWEL AY
     ]))

(defn grapheme-break? [code-point-1 code-point-2]
  (cond 
    (not (and code-point-1 code-point-2))
    true
    
    (and (cr? code-point-1) (lf? code-point-2))
    false
    
    (and 
      (any-p? [cr? lf? control-char?] code-point-1)
      (any-p? [cr? lf? control-char?] code-point-2))
    true
    
    ; TO DO HANGUL
    
    
    (extending-char? code-point-2)
    false
    
    (spacing-mark? code-point-2)
    false
    
    (prepending-char? code-point-1)
    false
    
    true
    true
    ))

(defn grapheme-split-pt [char-pair-seq current result]
  (let [[[c1 c2] & others] char-pair-seq]
    (if (nil? c1)
      (if (seq current)
        (conj result current)
        result)
      (let [n-current (conj current c1)]
        (if (grapheme-break? c1 c2)
          (recur others [] (conj result n-current))
          (recur others n-current result))))))
  

(defn grapheme-split [text]
  (let [pt (partition 2 1 (concat (seq text) [nil]))]
    (grapheme-split-pt pt [] [])))

(comment 
  
  (require '[utils.unicode :as uu] :reload)
  
  (require 
    '[utils.unicode :as uu]
    '[utils.map :as umap]
    '[utils.x.core :as u]
    :reload)
  
  (def UNICODE-DATA "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
 
  
  (def unicode (uu/load-characters LOCAL-UNICODE-DATA))
  (def major-categories (group-by uu/major-category unicode))
  (def minor-categories (group-by uu/categories unicode))
  
  (def major-cats-fname "resources/utils/unicode-major-categories.cljdat")
  (def cat-L-fname "resources/utils/unicode-L.cljdat")

  (def major-cats-strs (umap/mapvals uu/unicode-seqs major-categories))
  
  (u/write-pp major-cats-fname major-cats-strs)
  (u/write-pp cat-L-fname (:L major-cats-strs))
  
  (def blocks (uu/load-blocks LOCAL-UNICODE-BLOCKS))
  
  )


