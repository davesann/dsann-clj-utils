(ns utils.dictionary.lookup
  (require
    [utils.x.core :as u]
    [utils.map :as umap]
    [utils.seq :as useq]
    [utils.unicode :as uu]
    [clojure.string :as s]
    )
  )

(defn add [lookup-dict [key-seq value & others]]
  (if (empty? key-seq)
    lookup-dict
    (let [path (concat key-seq [:-value-])
          new-value (if-let [value-list (umap/hget lookup-dict path)]
                      (if (useq/not-in value value-list)
                        (conj value-list value)
                        value-list)
                      [value])
          new-dict (umap/hput lookup-dict path new-value)          
          ]
      (recur new-dict others))))


(defn add-unique [lookup-dict [key-seq value & others]]
  (if (empty? key-seq)
    lookup-dict
    (let [path (concat key-seq [:-value-])
          new-dict (umap/hput lookup-dict path value)          
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
  ([lookup-fn key-seq] 
    (all-matches lookup-fn key-seq 0 []))
  
  ([lookup-fn key-seq idx results]
    (if (empty? key-seq)
      results
      (let [new-results (let [found (lookup-fn key-seq)]
                          (if (empty? found)
                            results
                            (conj results [idx found])))
            ]
        (recur lookup-fn (rest key-seq) (inc idx) new-results)))))

(defn idx-pair-compare [[idx1  _]
                       [idx2  _]]
  (useq/r-compare 
    [first
     #(* -1 (second %))]
    idx1 idx2 
    ))



(defn process-matches 
  ([processed] (process-matches processed []))
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
                                           :lookups v}]))
                          found)
            n-results  (concat results sub-results)
            ]
        (recur remainder n-results)))))


(defn process [lookup-fn phrase]
  (let [matches (all-matches lookup-fn phrase)]
    (process-matches matches)))

                       
(defn phrase->word-table [lookup-fn phrase]
  (useq/tabulate-overlap (process lookup-fn phrase)))




(defn load-jp-dict []
  (let [data (read-string (slurp "/home/dave/work/data/dictionaries/jp/edict.cljdat"))]
    (reduce (fn [d entry]
              (-> d
                (add [(uu/grapheme-split (:primary entry)) entry])
                (add [(uu/grapheme-split (:pron entry)) entry])))
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
  '[utils.x.core :as u]
  '[utils.seq :as useq]
  '[utils.x.swingdraw :as d]
  :reload)

(require '[utils.dictionary.jp :as jp] :reload)

(def p pp/pprint)


;(def jp-dict (udl/load-jp-dict))
  

(def cn-dict (udl/load-cn-dict))
  

(def jp-txt "東日本大震災により、関東の超高層マンションの高層階では７割以上の住居でタンスや冷蔵庫、食器棚が転倒や移動したことが、東京理科大の調査でわかった。首都直下地震などでは、さらに大きな揺れが想定され、家具の固定などの対策が必要だとしている。")
(def jp-txt "東日本大震災により、関東の超高層マンション")

(def jp-txt  "東日本大震災により")

(def cn-txt "人民网北京11月10日电 （记者 王欲然）据河北卫视消息，本月6日，河北唐山滦南司各庄1名儿童在食用路边捡到的零食后身亡")
(def cn-txt "曹冲称象")
(defn cn-lookup [txt]
  (udl/lookup cn-dict  txt))

(p (udl/lookup cn-dict (seq cn-txt)))
(p (cn-lookup (seq cn-txt)))

(d/draw (take 5 cn-dict))

(d/draw (udl/all-matches cn-lookup (uu/grapheme-split cn-txt)))

(d/draw (udl/process-matches (udl/all-matches cn-lookup cn-txt)))

(d/draw (useq/tabulate-overlap (udl/process cn-lookup cn-txt)))


(d/draw (udl/phrase->word-table cn-lookup cn-txt))



(d/draw (udl/phrase->word-table jp/lookup-txt (seq jp-txt)))


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


