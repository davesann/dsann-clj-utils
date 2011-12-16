(ns utils.transliterate.jp1
  (:require 
    [clojure.string :as s]
    
    [utils.seq :as useq]
    [utils.map :as umap]
    [utils.unicode.jp :as ujp]
    
    [utils.x.core :as u]
    )
  )

(def kana->roma
  (let [m
        {
         [\あ \ア] "a"  [\い \イ] "i"  [\う \ウ] "u"  [\え \エ] "e"  [\お \オ] "o"
         [\か \カ] "ka" [\き \キ] "ki" [\く \ク] "ku" [\け \ケ] "ke" [\こ \コ] "ko" 
         [\さ \サ] "sa" [\し \シ] "si" [\す \ス] "su" [\せ \セ] "se" [\そ \ソ] "so" 
         [\た \タ] "ta" [\ち \チ] "ti" [\つ \ツ] "tu" [\て \テ] "te" [\と \ト] "to" 
         [\な \ナ] "na" [\に \ニ] "ni" [\ぬ \ヌ] "nu" [\ね \ネ] "ne" [\の \ノ] "no"  
         [\は \ハ] "ha" [\ひ \ヒ] "hi" [\ふ \フ] "hu" [\へ \ヘ] "he" [\ほ \ホ] "ho" 
         [\ま \マ] "ma" [\み \ミ] "mi" [\む \ム] "mu" [\め \メ] "me" [\も \モ] "mo" 
         [\ら \ラ] "ra" [\り \リ] "ri" [\る \ル] "ru" [\れ \レ] "re" [\ろ \ロ] "ro"
         
         [\が \ガ] "ga"	[\ぎ \ギ] "gi"	[\ぐ \グ] "gu"	[\げ \ゲ] "ge"	[\ご \ゴ] "go"	
         [\ざ \ザ] "za"	[\じ \ジ] "zi"	[\ず \ズ] "zu"	[\ぜ \ゼ] "ze"	[\ぞ \ゾ] "zo"	
         [\だ \ダ] "da"	[\ぢ \ヂ] "zi"	[\づ \ヅ] "zu"	[\で \デ] "de"	[\ど \ド] "do"	
         [\ば \バ] "ba"	[\び \ビ] "bi"	[\ぶ \ブ] "bu"	[\べ \ベ] "be"	[\ぼ \ボ] "bo"	
         [\ぱ \パ] "pa"	[\ぴ \ピ] "pi"	[\ぷ \プ] "pu"	[\ぺ \ペ] "pe"	[\ぽ \ポ] "po"	
         
         ["きゃ" "キャ"] "kya" ["きゅ" "キュ"] "kyu" ["きょ" "キョ"] "kyo"
         ["しゃ" "シャ"] "sya" ["しゅ" "シュ"] "syu" ["しょ" "ショ"] "syo"
         ["ちゃ" "チャ"] "tya" ["ちゅ" "チュ"] "tyu" ["ちょ" "チョ"] "tyo"
         ["にゃ" "ニャ"] "nya" ["にゅ" "ニュ"] "nyu" ["にょ" "ニョ"] "nyo"
         ["ひゃ" "ヒャ"] "hya" ["ひゅ" "ヒュ"] "hyu" ["ひょ" "ヒョ"] "hyo"
         ["みゃ" "ミャ"] "mya" ["みゅ" "ミュ"] "myu" ["みょ" "ミョ"] "myo"

         ["ぎゃ" "ギャ"] "gya"	["ぎゅ" "ギュ"] "gyu"	["ぎょ" "ギョ"] "gyo"
         ["じゃ" "ジャ"] "zya"	["じゅ" "ジュ"] "zyu"	["じょ" "ジョ"] "zyo"
         ["ぢゃ" "ヂャ"] "zya"	["ぢゅ" "ヂュ"] "zyu"	["ぢょ" "ヂョ"] "zyo"
         ["びゃ" "ビャ"] "bya"	["びゅ" "ビュ"] "byu"	["びょ" "ビョ"] "byo"
         ["ぴゃ" "ピャ"] "pya"	["ぴゅ" "ピュ"] "pyu"  ["ぴょ" "ピョ"] "pyo"

         [\や \ヤ] "ya" [\ゆ \ユ] "yu"  [\よ \ヨ] "yo" 
     
         ["りゃ" "リャ"] "rya" ["りゅ" "リュ"] "ryu" ["りょ" "リョ"] "ryo"
         [\わ \ワ] "wa" [\ゐ \ヰ] "i" [\ゑ \ヱ] "e" [\を \ヲ] "o" 
         [\ん \ン] "n"
         }
        
        m-h (umap/mapkeys #(let [h (first %)]
                             (if (char? h) [h] (seq h)))
              m)
        
        m-k (umap/mapkeys #(let [k (second %)]
                           (if (char? k) [k] (seq k)))
              m)
        ]
    (merge m-h m-k)))

(defn n-str [c]
  (if (useq/in [\a \e \i \o \u \y] c)
    "n'"
    "n"))

(defn sokuon-char? [c]
 (= c \っ))

(defn n-char? [c]
  (some #{c} [\ん \ン]))

(defn merging-char? [c]
  (some #{c} [\ャ \ゅ \ゃ \ュ \ょ \ョ]))

(defn char-type [c]
  (cond
    (sokuon-char? c)  :sokuon
    (n-char? c)       :n
    (ujp/hiragana? c) :hiragana
    (ujp/katakana? c) :katakana
    true :other))


(defn partition-jp 
  "partition jp txt for transliteration

よんだ本にすばらしい絵があった。 きゃい -> 

([:hiragana \\よ] [:n \\ん \\だ] [:hiragana \\だ] [:other \\本] [:hiragana \\に] 
 [:hiragana \\す] [:hiragana \\ば] [:hiragana \\ら] [:hiragana \\し] [:hiragana \\い] 
 [:other \\絵] [:hiragana \\が] [:hiragana \\あ] [:sokuon \\っ \\た] [:hiragana \\た] 
 [:other \\。 \\space] [:merge \\き \\ゃ] [:hiragana \\い])
"
  ([txt-seq] (partition-jp txt-seq :none [] []))
  ([[c & txt-seq] last-c-type current result]
    (if (nil? c)
      (remove empty? (conj result current))
      (let [c-type (char-type c)]
          (cond
            (= :other c-type)
            (if (= :other last-c-type)
              (recur txt-seq c-type (conj current c) result)
              (recur txt-seq c-type [c-type c] (conj result current)))
            
            (some #{c-type} [:sokuon :n])
            (let [[f s & r] txt-seq]
              (if (merging-char? s)
                (recur r :none [] (conj result current [c-type c f] [:merge f s]))
                (recur txt-seq :none [] (conj result current [c-type c f]))))
            
            (some #{c-type} [:hiragana :katakana])
            (let [[f & r] txt-seq]
              (if (merging-char? f)
                (recur r :none [] (conj result current [:merge c f]))
                (recur txt-seq c-type [c-type c] (conj result current))))
            
            true :error
            )))))

(defn ->romanji-annotate [txt]
  "annotate partitioned txt with romanji transliteration

よんだ本にすばらしい絵があった。 きゃい ->

([\\よ \"yo\"] [\\ん \"n\"] [\\だ \"da\"] [\"本\" \"本\"] [\\に \"ni\"] [\\す \"su\"] 
 [\\ば \"ba\"] [\\ら \"ra\"] [\\し \"si\"] [\\い \"i\"] [\"絵\" \"絵\"] [\\が \"ga\"] [\\あ \"a\"] 
 [\\っ \\t] [\\た \"ta\"] [\"。 \" \"。 \"] [(\\き \\ゃ) \"kya\"] [\\い \"i\"])

"
  (mapcat
    (fn [[char-type & chars]]
        (case char-type
          :hiragana 
          (map (fn [c] [c (kana->roma [c])]) chars)
        
          :katakana 
          (map (fn [c] [c (kana->roma [c])]) chars)

          :merge 
          [[chars (kana->roma chars)]]
          
          :n        
          (let [[f & r] chars
                trans-str (kana->roma r)
                n (n-str (first trans-str))
                ]
            [[f n]])
          
          :sokuon  
          (let [[f & r] chars
                trans-str (kana->roma r)]
            [[f (first trans-str)]])
          
          :other
          (let [v (apply str chars)]
            [[v v]])
          ))
    (partition-jp (seq txt))))
  
(defn ->romanji [txt]
  "transliterate to a list of items

よんだ本にすばらしい絵があった。 きゃい ->

(\"yo\" \"n\" \"da\" \"本\" \"ni\" \"su\" \"ba\" \"ra\" \"si\" \"i\" \"絵\"
 \"ga\" \"a\" \t \"ta\" \"。 \" \"kya\" \"i\")

"
  (map second (->romanji-annotate txt)))

(defn ->romanji-str
  "transliteration to a string 
   with transliterated items separated by sep

よんだ本にすばらしい絵があった。 きゃい ->

\"yo-n-da-本-ni-su-ba-ra-si-i-絵-ga-a-t-ta-。 -kya-i\"
"
  ([txt] (->romanji-str txt "-"))
  ([txt sep]
    (s/join sep (->romanji txt))))


  

(comment
  
  (t/->romanji-annotate "よんだんよ本にすばらしい絵があった。 balh jasj asdkk ;lasd;l \n\n dkmlkdvms")

  
(defn ->romanji [txt]
  (map second (->romanji-annotate txt)))

 
  (require '[transliterate.jp :as t] :reload-all)
  
  (require 
    '[clojure.string :as s]
    '[net.cgrand.parsley :as p]
     
    '[clojure.set :as cset]
    '[clojure.string :as s]
    
    '[utils.seq :as useq]
    '[utils.map :as um]
    
    '[utils.x.core :as u]
    '[utils.unicode.jp :as ujp]
    '[clojure.pprint :as pp] :reload-all)
  
  (def p pp/pprint)

  (def sample-text "よんだ本本んよ本にすばらしい絵があった。")
  
  (def sample-text "よんだ本本んよ本にすばらしい絵があった。 balh jasj asdkk ;lasd;l \n\n dkmlkdvms")
  
  (def sample-text "いつもいつもはっきりさせなきゃいけなくて。")

  (def sample-text "きゃいけなくて。")
  
  (t/->romanji-annotate sample-text)

  (t/->romanji-str
    "ぼくが6つのとき、よんだ本にすばらしい絵があった。 きゃい 『ぜんぶほんとのはなし』という名まえの、しぜんのままの森について書かれた本で、そこに、ボアという大きなヘビがケモノをまるのみしようとするところがえがかれていたんだ。だいたいこういう絵だった。"    
    )
  
  (def parser 
    (p/parser 
      {:main [:expr+] :root :root }
      :expr  #{:a-or-b :a-and-c}
      :a-or-b    \a 
      :a-and-c   [\a \c]
      ))
  
  (p  (parser "acbabbac"))

  
)


