(ns utils.dictionary.lookup-1
  (require
    [utils.x.core :as u]
    [utils.map :as um]
    [utils.seq :as useq]
    [utils.unicode :as uu]
    [clojure.string :as s]
    )
  )

(defn add [lookup-dict [key-seq value & others]]
  (if (empty? key-seq)
    lookup-dict
    (let [path (concat key-seq [:-value-])
          new-value (if-let [old-value (um/hget lookup-dict path)]
                      (conj old-value value)
                      [value])
          new-dict (um/hput lookup-dict path new-value)          
          ]
      (recur new-dict others))))

(defn lookup 
  ([lookup-dict key-seq] 
    (lookup lookup-dict key-seq [] {}))
  
  ([lookup-dict [k & key-seq] path result]
    (let [n-result (if-let [v (:-value- lookup-dict)]
                     (conj result [path v])
                     result)]
      (if (nil? k)
        n-result
        (if-let [n-dict (lookup-dict k)]
          (recur n-dict key-seq (conj path k) n-result)
          n-result
          ))
      )))

(defn all-matches 
  ([dict key-seq] 
    (all-matches dict key-seq 0 []))
  
  ([dict key-seq idx results]
    (if (empty? key-seq)
      results
      (let [new-results (let [found (lookup dict key-seq)]
                          (if (empty? found)
                            results
                            (conj results [idx found])))
            ]
        ;[:recur results])))
        (recur dict (rest key-seq) (inc idx) new-results)))))

(defn process [lookup-dict phrase]
  (let [matches (all-matches lookup-dict (uu/grapheme-split phrase))]
    (sort-by (fn [[idx _]] idx)
      (um/mapvals (fn [m] (um/mapvals 
                            ;#(s/join ""  (apply concat %))      ; join the chars to a word 
                            #(map :eentry %)    ; select only the english entry
                            m))
        matches
      )
    ))
)

(defn idx-pair-compare [[idx1  _]
                       [idx2  _]]
  (useq/r-compare idx1 idx2 
    [first
     #(* -1 (second %))]))
          
                    

(defn process-more 
  ([processed] (process-more processed []))
  ([[[idx found] & remainder] results]
    (if (nil? idx) 
      (sort idx-pair-compare results)
      (let [sub-results (map (fn [[k v]]
                               (let [l (count k)
                                     e (+ idx l)
                                     ]
                                 [[idx e] {:idx idx
                                           :end-idx e
                                           :len l 
                                           :th k 
                                           :en v}]))
                          found)
            n-results  (concat results sub-results)
            ]
        (recur remainder n-results)))))

(defn partition-overlap 
  "creates a list of lists of no overlapping words. Longest words go first"
  ([indexed-seq]
    (partition-overlap indexed-seq [] [] []))
  ([indexed-seq held iresult fresult]
    (if (empty? indexed-seq)
      (if (empty? held)
        fresult
        (recur held [] [] (conj fresult iresult)))
      (let [[item & remainder] indexed-seq
            [[s e] v] item
            [n-held new-indexed-seq] (split-with (fn [[[s1 _] _]] (> e s1)) remainder)
            ]
        (recur new-indexed-seq (concat held n-held) (conj iresult v) fresult)
        ))))
              
(defn phrase->word-table [lookup-dict phrase]
  (partition-overlap (process-more (process lookup-dict phrase))))




(defn load-thai-dict []
  (let [data (read-string (slurp  "/home/dave/work/data/dictionaries/th/telex.2.cljdat"))]
    (reduce (fn [d entry]
              (add d [(uu/grapheme-split (:tsearch entry)) entry]))
      {}
      data)))

(defn load-jp-dict []
  (let [data (read-string (slurp "/home/dave/work/data/dictionaries/jp/edict.cljdat"))]
    (reduce (fn [d entry]
              (-> d
                (add [(uu/grapheme-split (:primary entry)) entry])
                (add [(uu/grapheme-split (:pron entry)) entry])))
      {}
      data)))
                  

(comment  
;(pp/pprint




(def lp "เมื่อ ตอนฉันอายุได้ ๖ ขวบ ฉันได้เห็นรูปภาพจับใจรูปหนึ่งในหนังสือเกี่ยวกับป่าดงดิบชื่อว่า \"ประวัติชีวิตธรรมชาติ\" รูปนั้นเป็นรูปงูเหลือมกำลังกลืนสัตว์ป่า นี่คือรูปลอกของภาพนั้น")
(def news "บ้านเป็นปัจจัยพื้นฐาน ที่สำคัญอย่างหนึ่งในการดำรงชีวิต ทุกคนต้องการมีบ้านเป็นของตนเองไม่ว่าจะเป็นบ้านเล็ก บ้านใหญ่ ก็สามารถที่จะให้ความสุขแก่ทุกคนในครอบครัวได้ ปัจจัยที่จะทำให้บ้านน่าอยู่หรือไม่นั้น ขึ้นอยู่กับสภาพแวดล้อมทั้งที่ตั้งซึ่งถูกทิศทาง การออกแบบ ที่เหมาะสม เพื่อนบ้าน และสมาชิกทุกคนในครอบครัว ซึ่งมีความเข้าใจซึ่งกันและกัน")
(def bl "ผมจะพาคุณไปฝรั่งเศสค่ำนี้  ไปโบโลญดูทัศนียภาพ แล้วกลับมาวันอังคารหน้าได้ไหม")
(def bl-t "Phom ja pha khun pai pha-hrung-set khum-ni pai boh-lone du thad-sa-ni-ya-pab laew klab ma wan-ung-karn hna dai hmai?")
(def bl-e "I’m taking you to France this very evening - to Boulogne, to see the sights. All right to come back next Tuesday?")

(def blah "กำลังมอบกล่องข้าวให้กับเด็กที่ยื่นมือมารอรับ ในระหว่างลงพื้นทีี่เขตบางพลัด พร้อมทั้งเตรียมมอบถุงยังชีพซึ่งสกรีนชื่อบนถุงว่า ฯพณฯ นายกรัฐมนตรีหญิงยิ่งลักษณ์ ชินวัตร และ ฯพณฯ วรวัจน์ เอื้ออภิญญกุล รัฐมนตรีว่าการกระทรวงศึกษาธิการ")


(def r
  (hc/html
    [:body 
     (txt->hiccup lp #(map (fn [x] [:p x]) (:en %)))
     (txt->hiccup lp :th)
     ]
    )
  )
;)
)


