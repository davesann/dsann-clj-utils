(ns utils.transliterate.jp
  (:require 
    [clojure.string :as s]
    
    [utils.seq :as useq]
    [utils.map :as umap]
    [utils.unicode.jp :as ujp]
    
    [utils.x.core :as u]
    )
  )

(defn vowel? [c]
  (useq/in [\a \e \i \o \u \y \A \E \I \O \U \Y] c))

(defn n-str [n c]
  (case n
    \ん (if (vowel? c) "n'" "n")
    \ン (if (vowel? c) "N'" "N")))

(defn n-str-k [n n1 n2]
  (if (and (= n1 \') (vowel? n2))
    (case n 
      \n \ん
      \N \ン
      nil)))

(defn sokuon-char? [c]
 (= c \っ))

(defn kana-n-char? [c]
  (some #{c} [\ん \ン]))

(defn roma-n-char? [c]
  (some #{c} [\n \N]))


(defn merging-char? [c]
  (some #{c} [\ャ \ゅ \ゃ \ュ \ょ \ョ]))

(defn char-type [c]
  (cond
    (sokuon-char? c)  :sokuon
    (kana-n-char? c)  :n
    (ujp/hiragana? c) :hiragana
    (ujp/katakana? c) :katakana
    true :other))

      
(defn- add-1 [lookup-dict [key-seq value & others]]
  (if (empty? key-seq)
    lookup-dict
    (let [path (concat key-seq [:-value-])
          new-dict (umap/hput lookup-dict path value)          
          ]
      (recur new-dict others))))

(defn pairwise-swap
  "applies only to even number of items

   (pairwise-swap [1 2 3 4 5 6])
   [2 1 4 3 6 5]
"
  ([a-seq] (pairwise-swap a-seq []))
  ([[a b & others] result]
    (if (and (nil? others) (nil? a))
      result
      (recur others (conj result b a)))))
                     


(def kana->roma-data-hepburn
  [[\あ \ア] "a"  [\い \イ] "i"  [\う \ウ] "u"  [\え \エ] "e"  [\お \オ] "o"
   [\か \カ] "ka" [\き \キ] "ki" [\く \ク] "ku" [\け \ケ] "ke" [\こ \コ] "ko" 
   [\さ \サ] "sa" [\し \シ] "shi" [\す \ス] "su" [\せ \セ] "se" [\そ \ソ] "so" 
   [\た \タ] "ta" [\ち \チ] "chi" [\つ \ツ] "tsu" [\て \テ] "te" [\と \ト] "to" 
   [\な \ナ] "na" [\に \ニ] "ni" [\ぬ \ヌ] "nu" [\ね \ネ] "ne" [\の \ノ] "no"  
   [\は \ハ] "ha" [\ひ \ヒ] "hi" [\ふ \フ] "fu" [\へ \ヘ] "he" [\ほ \ホ] "ho" 
   [\ま \マ] "ma" [\み \ミ] "mi" [\む \ム] "mu" [\め \メ] "me" [\も \モ] "mo" 
   [\ら \ラ] "ra" [\り \リ] "ri" [\る \ル] "ru" [\れ \レ] "re" [\ろ \ロ] "ro"
   
   [\が \ガ] "ga"	[\ぎ \ギ] "gi"	[\ぐ \グ] "gu"	[\げ \ゲ] "ge"	[\ご \ゴ] "go"	
   [\ざ \ザ] "za"	[\じ \ジ] "ji"	[\ず \ズ] "zu"	[\ぜ \ゼ] "ze"	[\ぞ \ゾ] "zo"	
   [\だ \ダ] "da"	[\ぢ \ヂ] "ji"	[\づ \ヅ] "zu"	[\で \デ] "de"	[\ど \ド] "do"	
   [\ば \バ] "ba"	[\び \ビ] "bi"	[\ぶ \ブ] "bu"	[\べ \ベ] "be"	[\ぼ \ボ] "bo"	
   [\ぱ \パ] "pa"	[\ぴ \ピ] "pi"	[\ぷ \プ] "pu"	[\ぺ \ペ] "pe"	[\ぽ \ポ] "po"	
   
   ["きゃ" "キャ"] "kya" ["きゅ" "キュ"] "kyu" ["きょ" "キョ"] "kyo"
   ["しゃ" "シャ"] "sha" ["しゅ" "シュ"] "shu" ["しょ" "ショ"] "sho"
   ["ちゃ" "チャ"] "cha" ["ちゅ" "チュ"] "chu" ["ちょ" "チョ"] "cho"
   ["にゃ" "ニャ"] "nya" ["にゅ" "ニュ"] "nyu" ["にょ" "ニョ"] "nyo"
   ["ひゃ" "ヒャ"] "hya" ["ひゅ" "ヒュ"] "hyu" ["ひょ" "ヒョ"] "hyo"
   ["みゃ" "ミャ"] "mya" ["みゅ" "ミュ"] "myu" ["みょ" "ミョ"] "myo"

   ["ぎゃ" "ギャ"] "gya"	["ぎゅ" "ギュ"] "gyu"	["ぎょ" "ギョ"] "gyo"
   ["じゃ" "ジャ"] "ja"	["じゅ" "ジュ"] "ju"	["じょ" "ジョ"] "jo"
   ["ぢゃ" "ヂャ"] "ja"	["ぢゅ" "ヂュ"] "ju"	["ぢょ" "ヂョ"] "jo"
   ["びゃ" "ビャ"] "bya"	["びゅ" "ビュ"] "byu"	["びょ" "ビョ"] "byo"
   ["ぴゃ" "ピャ"] "pya"	["ぴゅ" "ピュ"] "pyu"  ["ぴょ" "ピョ"] "pyo"
   ["りゃ" "リャ"] "rya" ["りゅ" "リュ"] "ryu" ["りょ" "リョ"] "ryo"
   
   [\や \ヤ] "ya" [\ゆ \ユ] "yu"  [\よ \ヨ] "yo" 
  
   [\わ \ワ] "wa" [\を \ヲ] "wo"
   ;[\ゐ \ヰ] "i" [\ゑ \ヱ] "e"
   [\ん \ン] "n" [\ん \ン] "n'"
   ])


(def kana->roma-data-kunrei-shiki
  [[\あ \ア] "a"  [\い \イ] "i"  [\う \ウ] "u"  [\え \エ] "e"  [\お \オ] "o"
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
   [\わ \ワ] "wa" [\を \ヲ] "wo"
   ;[\ゐ \ヰ] "i" [\ゑ \ヱ] "e"
   [\ん \ン] "n" [\ん \ン] "n'"
   ])

(def kana-data kana->roma-data-hepburn)

(def kana->roma-dict
  (let [
        m-h (map #(if (sequential? %)
                    (let [h (first %)]
                      (if (char? h) [h] (seq h)))
                    %)
              kana-data)
        
       m-k (map #(if (string? %) 
                   (s/upper-case %)
                   (let [k (second %)]
                     (if (char? k) [k] (seq k))))
             kana-data)
       ]
    (-> {}
      (add-1 m-h)
      (add-1 m-k)
      )))


(def roma->kana-dict
  (let [m-h (map #(if (sequential? %) (first %) %)
                 kana-data)
        
       m-k (map #(if (string? %) 
                   (s/upper-case %)
                   (second %))
             kana-data)
       ]
    (-> {}
      (add-1 (pairwise-swap m-h))
      (add-1 (pairwise-swap m-k))
      )))

(defn match-case [c1 c]
  (if (Character/isUpperCase c1)
    (Character/toUpperCase c)
    (Character/toLowerCase c)))

(defn roma-rules [path char-seq]
  (let [[c1 & cs1] char-seq]
    (if (empty? path)
      (let [[c2 & cs2] cs1] 
        (cond
          (not (vowel? c1))
          (if (= c1 c2)
            {:path [c1] :lookup \っ :key-seq-remaining cs1})
          
          (roma-n-char? c1)
          (let [[c3 & cs3] cs2]
            (if (and (= c2 \') (vowel? c3))
              {:path [c1 c2] :lookup (match-case c1 \n) :key-seq-remaining cs2}
              {:path [c1] :lookup (match-case c1 \n) :key-seq-remaining cs1}))
          )))))

(declare lookup-kana)

(defn kana-rules [path char-seq]
  (let [[c1 & cs1] char-seq]
    (if (empty? path)
      (let [c-type (char-type c1)]
        (if (useq/in [:sokuon :n] c-type)
          (let [next-l (lookup-kana cs1)]
            (case c-type
              :sokuon
              {:path [c1] :lookup (first (:lookup next-l)) :key-seq-remaining cs1}    
              :n
              {:path [c1] :lookup (n-str c1 (first (:lookup next-l))) :key-seq-remaining cs1}
              )))))))
    
    
(defn lookup-after-rules 
  ([rules dict char-seq] (lookup-after-rules rules dict char-seq []))
  ([rules dict char-seq path]
    (or 
      (rules path char-seq)
      (let [[c1 & cs1] char-seq]  
        (if-let [n-dict (dict c1)]
          (recur rules n-dict cs1 (conj path c1))
          (if (empty? path)
            nil
            (if-let [l (:-value- dict)]
              {:path path :lookup l :key-seq-remaining char-seq}
              nil
              )))))))
    
(defn lookup-roma
  ([char-seq] 
    (lookup-after-rules 
      roma-rules roma->kana-dict char-seq [])))
  
(defn lookup-kana
  ([char-seq] 
    (lookup-after-rules 
      kana-rules kana->roma-dict char-seq [])))
  



(defn transliterate-annotate- [lookup-fn tseq result]
  (if (empty? tseq)
    result
    (if-let [lu (lookup-fn tseq)]
      (let [{p :path l :lookup r :key-seq-remaining} lu]
        (recur lookup-fn r (conj result [p l])))
      (let [[f & r] tseq]
        (recur lookup-fn r (conj result [f f]))))))
  
(defn transliterate-annotate [lookup-fn txt]
  (let [tseq (seq txt)]
    (transliterate-annotate- lookup-fn tseq [])))

(defn transliterate [lookup-fn txt]
  (map second (transliterate-annotate lookup-fn txt)))

(defn transliterate-str 
  ([lookup-fn txt] (transliterate-str lookup-fn "" txt))
  ([lookup-fn sep txt]
    (s/join sep (transliterate lookup-fn txt))))

(defn kana->roma [txt]
  (transliterate lookup-kana txt))

(defn roma->kana [txt]
  (transliterate lookup-roma txt))


(defn kana->roma-str 
  ([txt] (kana->roma-str "" txt))
  ([sep txt]
    (s/join sep (kana->roma txt))))

(defn roma->kana-str 
  ([txt] (roma->kana-str "" txt))
  ([sep txt]
    (s/join sep (roma->kana txt))))




(comment
  
 
  (require '[utils.transliterate.jp :as t] :reload-all)
  
  (require 
    '[clojure.string :as s]
     
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

  (def sample-txt "それまで、ぼくはずっとひとりぼっちだった。だれともうちとけられないまま、6年まえ、
ちょっとおかしくなって、サハラさばくに下りた。ぼくのエンジン のなかで、なにかがこわれていた。ぼくには、
みてくれるひとも、おきゃくさんもいなかったから、なおすのはむずかしいけど、ぜんぶひとりでなん
とかやって みることにした。それでぼくのいのちがきまってしまう。のみ水は、たった7日ぶんしかな
かった。
")
  
  (def sample-txt "ちょっとおかしくなって")
  
  
  sample-txt
  (t/roma->kana-str (t/kana->roma-str sample-txt))
    
  (= sample-txt (t/roma->kana-str (t/kana->roma-str sample-txt)))
  
  (defn compare-strings [s1 s2]
    (remove nil? (map
                   (fn [c1 c2]
                     (if (not (= c1 c2))
                       [c1 c2]))
                   s1 s2)))
  
)


