(ns utils.dictionary.lookup
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
        (recur dict (rest key-seq) (inc idx) new-results)))))

(defn process [lookup-dict phrase]
  (let [matches (all-matches lookup-dict (uu/grapheme-split phrase))]
    (sort-by (fn [[idx _]] idx)
      (um/mapvals (fn [m] (um/mapvals 
                            #(set (apply concat (map :defs %)))  ; select only definitions
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
                                           :match k 
                                           :defs v}]))
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


(defn load-cn-dict []
  (let [data (read-string (slurp "/home/dave/work/data/dictionaries/cn/cedict.cljdat"))]
    (reduce (fn [d entry]
              (-> d
                (add [(uu/grapheme-split (:trad entry)) entry])
                (add [(uu/grapheme-split (:simp entry)) entry])
                (add [(s/split (:pinyin entry) #"\s+") entry])
                ))
      {}
      data)))
                  



(comment  

(def lp "เมื่อ ตอนฉันอายุได้ ๖ ขวบ ฉันได้เห็นรูปภาพจับใจรูปหนึ่งในหนังสือเกี่ยวกับป่าดงดิบชื่อว่า \"ประวัติชีวิตธรรมชาติ\" รูปนั้นเป็นรูปงูเหลือมกำลังกลืนสัตว์ป่า นี่คือรูปลอกของภาพนั้น")
(def news "บ้านเป็นปัจจัยพื้นฐาน ที่สำคัญอย่างหนึ่งในการดำรงชีวิต ทุกคนต้องการมีบ้านเป็นของตนเองไม่ว่าจะเป็นบ้านเล็ก บ้านใหญ่ ก็สามารถที่จะให้ความสุขแก่ทุกคนในครอบครัวได้ ปัจจัยที่จะทำให้บ้านน่าอยู่หรือไม่นั้น ขึ้นอยู่กับสภาพแวดล้อมทั้งที่ตั้งซึ่งถูกทิศทาง การออกแบบ ที่เหมาะสม เพื่อนบ้าน และสมาชิกทุกคนในครอบครัว ซึ่งมีความเข้าใจซึ่งกันและกัน")
(def bl "ผมจะพาคุณไปฝรั่งเศสค่ำนี้  ไปโบโลญดูทัศนียภาพ แล้วกลับมาวันอังคารหน้าได้ไหม")
(def bl-t "Phom ja pha khun pai pha-hrung-set khum-ni pai boh-lone du thad-sa-ni-ya-pab laew klab ma wan-ung-karn hna dai hmai?")
(def bl-e "I’m taking you to France this very evening - to Boulogne, to see the sights. All right to come back next Tuesday?")

(def blah "กำลังมอบกล่องข้าวให้กับเด็กที่ยื่นมือมารอรับ ในระหว่างลงพื้นทีี่เขตบางพลัด พร้อมทั้งเตรียมมอบถุงยังชีพซึ่งสกรีนชื่อบนถุงว่า ฯพณฯ นายกรัฐมนตรีหญิงยิ่งลักษณ์ ชินวัตร และ ฯพณฯ วรวัจน์ เอื้ออภิญญกุล รัฐมนตรีว่าการกระทรวงศึกษาธิการ")


(require 
  '[utils.dictionary.lookup :as udl]
  '[clojure.pprint :as pp]
  '[utils.unicode :as uu]
  
  :reload)

(def p pp/pprint)

(if (not (find (ns-interns *ns*) 'jp-dict))
  (def jp-dict (udl/load-jp-dict)))
  

(if (not (find (ns-interns *ns*) 'cn-dict))
  (def cn-dict (udl/load-cn-dict)))
  

(def jp-txt "東日本大震災により、関東の超高層マンションの高層階では７割以上の住居でタンスや冷蔵庫、食器棚が転倒や移動したことが、東京理科大の調査でわかった。首都直下地震などでは、さらに大きな揺れが想定され、家具の固定などの対策が必要だとしている。")
(def jp-txt "東日本大震災により、関東の超高層マンション")

(def jp-txt  "東日本大震災により")

(def cn-txt "人民网北京11月10日电 （记者 王欲然）据河北卫视消息，本月6日，河北唐山滦南司各庄1名儿童在食用路边捡到的零食后身亡")

(p (udl/lookup cn-dict (uu/grapheme-split cn-txt)))
(p (udl/process-more (udl/process cn-dict cn-txt)))

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


