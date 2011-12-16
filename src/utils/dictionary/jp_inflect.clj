(ns utils.dictionary.jp-inflect
  (:require 
    [clojure.string :as s]
    [utils.map :as umap]
    [utils.seq :as useq]
    [utils.x.core :as u]
    [utils.logic :as ul]
    [utils.unicode.jp :as ujp]
    [utils.dictionary.lookup :as udl]
    [utils.transliterate.jp :as jpt]
    )
  )




;; Inspired by, with thanks to 
;;  Ed Hally http://halley.cc/code/?python/nihongo.py

(defn leader [txt]
  (first (partition-by ujp/char-class txt)))

(defn divide [word]
  (let [[hiragana others] (split-with ujp/hiragana? (reverse word))]
    [(reverse others) (reverse hiragana)]))


(def pos 
  #{:v5r  :v5s :v5g :v5m :v5n 
    :v5t  :v5u :v5b :v5k :v1 
    :vs-s :adj})

(def stems 
  {:v1 "ru"  :v5r "ru" :v5ri "ru" :v5s  "su"  :v5g "gu"
   :v5m "mu" :v5n "nu" :v5t  "tsu"  :v5u "u"
   :v5b "bu" :v5k "ku" :vs_s "suru" :adj "i" })

(def particles ["ha" "ga" "no" "ni" "de" 
                "mo" "de" "ka" "yo" "ne" 
                "wa" "na" "ze" "to" ])

(def stems-kana
  {:v5b "ぶ" :v1 "る" :v5u "う" :v5s "す" :v5t "つ" :v5r "る" :v5r-i "る"
   :v5k "く" :v5m "む" :v5n "ぬ" :v5g "ぐ" :vs_s "する" :adj "い"})

(def particles-kana (map jpt/roma->kana-str particles))

(def inflections-base 
  {
   ;; # The adj-i forms.
   :adj {"non-past"          ["i"]
         "past"              ["katta"]
         "negative non-past" ["kunai"]
         "negative past"     ["kunakatta"]
         }
   
   ;; # The vs-* forms (<noun> suru).
   :vs-s {"non-past" ["suru" "shimasu"]
          "past"     ["shita" "shimashita"]
          "te-form"  ["shite" "shimashite"]
          }
   
   ; The v1 verb forms (ichidan).
   ; The "form" notation indicates a special form
   ; linguistically special but not programmatically.
   :v1 {"non-past"          ["ru" "masu" ]
        "past"              ["tta" "mashita" ]
        "te-form"           ["te" "mashite" "te-form"]
        "progressive"       ["teiru" "mashiteiru" ]
        "conditional"       ["tara" "mashitara" ]
        "provisional"       ["reba" "masunaraba" "maseba" ]
        "potential"         ["reru" "remasu" ]
        "passive"           ["rareru" "raremasu"]
        "causitive"         ["raseru" "sasemasu" ]
        "direct causitive"  ["rasu" "sashimasu" ]
        "causative-passive" ["rasarareru" "rasareru" 
                             "saseraremasu" "sasaremasu" ]
        "volitional"        ["rou" "mashou" ]
        "hortative"         ["ruyounishou" "rukotonishou" 
                             "ruyounishimashou" "rukotonishimashou" ]
        "conjectural"       ["rudarou" "rudeshou" ]
        "alternative"       ["ruttari" "mashitari" ]
        "imperative"        ["re" "nasai" ]
        "correlational"     ["runonara" ]
        "descriptive"       ["rukoto" ]
        
        ;# V1 +-
        "negative non-past"    ["nai" "nu" "zuni" "masen" ]
        "negative past"        ["nakatta" "masendeshita" ]
        "negative te-form"     ["nakute" "naide" "masende" ]
        "negative progressive" ["nakuteiru" "masendeiru" ]
        "negative conditional" ["nakattara" "masendeshitara" ]
        "negative provisional" ["nakereba" "masennaraba" ]
        "negative potential"   ["renai" "remasen" ]
        "negative passive"     ["rarenai" "raremasen" ]
        "negative causative"   ["sasenai" "sasanai" "sasemasen" "sashimasen" ]
        "negative causative-passive" ["saserarenai" "sasarenai" 
                                      "saseraremasen" "sasaremasen" ]
        "negative volitional"  ["mai" "masumai" ]
        "negative hortative"   ["naiyounishou" "naikotonishou" 
                                "naiyounishimashou" "naikotonishimashou" ]
        "negative conjectural" ["naiderou" "ranaideshou" ]
        "negative alternative" ["nakattari" "masendeshitari" ]
        "negative imperative"  ["runa" "nasaruna" ]
        }
   
   
   ;; # The v5r form is the basis for all godan (v5_) forms.
   :v5r {"correlational"        ["runonara"]
         "non-past"             ["ru" "rimasu"]
         "past"                 ["tta" "rimashita"]
         "te-form"              ["tte" "rimashite"]
         "progressive"          ["tteiru" "rimashiteiru"]
         "conditional"          ["ttara" "rimashitara"]
         "provisional"          ["reba" "rimasunaraba" "rimaseba"]
         "potential"            ["reru" "remasu"]
         "descriptive"          ["rukoto"]
         "passive"              ["rareru" "raremasu"]
         "causitive"            ["raseru" "rasemasu"]
         "direct causitive"     ["rasu" "rashimasu"]
         "causative-passive"    ["raserareru" "rasareru" "raseraremasu" "rasaremasu"]
         "volitional"           ["rou" "rimashou"]
         "hortative"            ["ruyounishou"
                                 "rukotonishou"
                                 "ruyounishimashou"
                                 "rukotonishimashou"]
         "conjectural"          ["rudarou" "rudeshou"]
         "alternative"          ["ruttari" "rimashitari"]
         "imperative"           ["re" "rinasai"]
         "negative non-past"    ["ranai" "ranu" "razuni" "rimasen"]
         "negative past"        ["ranakatta" "rimasendeshita"]
         "negative te-form"     ["ranakute" "ranaide" "rimasende"]
         "negative progressive" ["ranakuteiru" "rimasendeiru"]
         "negative conditional" ["ranakattara" "rimasendeshitara"]
         "negative provisional" ["renakereba" "rimasennaraba"]
         "negative potential"   ["renai" "remasen"]
         "negative passive"     ["rarenai" "raremasen"]
         "negative causitive"   ["rasenai" "rasemasen"]
         "negative causative-passive" ["raserarenai" "rasarenai" 
                                       "raseraremasen" "rasaremasen"]
         "negative volitional"  ["rumai" "rimasumai"]
         "negative hortative"   ["ranaiyounishou"
                                 "ranaikotonishou"
                                 "ranaiyounishimashou"
                                 "ranaikotonishimashou"]
         "negative conjectural" ["ranaidarou" "ranaideshou"]
         "negative alternative" ["ranakattari" "rimasendeshitari"]
         "negative imperative"  ["runa" "rinasaruna"]
         }
   })

(def v5-templates
  {
   :r-i {"negative non-past"    ["ranu" "razuni" "rimasen"]  ;nai
         "negative past"        ["rimasendeshita"] ; nakatta
         } 
   
   :s {"te-form"         ["shite" "shimashite"]
       "past"            ["shita" "shimashita"]
       "progressive"     ["shiteiru" "shimashiteiru"]
       "past polite (?)" ["shittara" "shimashitara"]}
   
   :k {"te-form"         ["ide" "kimashite"]
       "past"            ["ida" "kimashita" ]
       "progressive"     ["ideiru" "kimashiteiru"]
       "past polite (?)" ["idara" "kimashitara"]
       }
   
   :g {"te-form"         ["ide"    "gimashite"]
       "past"            ["ida"    "gimashita"]
       "progressive"     ["ideiru" "gimashiteiru"]
       "past polite (?)" ["idara"  "gimashitara" ]
       }
   
   :b {"te-form"         ["nde" "bimashite"]
       "past"            ["nda" "bimashita"]
       "progressive"     ["ndeiru" "bimashiteiru"]
       "past polite (?)" ["ndara" "bimashitara"]
       }
   
   :m {"te-form"         ["nde"    "mimashite"]
       "past"            ["nda"    "mimashita"]
       "progressive"     ["ndeiru" "mimashiteiru"]
       "past polite (?)" ["ndara"  "mimashitara"]
       }
   
   :n {"te-form"         ["nde" "nimashite"]
       "past"            ["nda" "nimashita"]
       "progressive"     ["ndeiru" "nimashiteiru"]
       "past polite (?)" ["ndara" "nimashitara"]
       }
   
   :t {}
   
   :u {}
   })
  

(defn- expand-template [reference v5-index v5-endings templates template-key]
  (let [template (templates template-key)
        k (keyword (str "v5" (name template-key)))
        v (umap/mapvals
            (fn [inflections]
              (map (fn [inflection]
                     (if-let [r (v5-index (subs inflection 0 2))]
                       (str (nth (template-key v5-endings) r) (subs inflection 2))
                       inflection))
                inflections))
            (merge reference template))
        ]
    {k v}))

;; # Other v5_ varieties differ from v5r in te-form and past only.
;; # We derive these.
(def inflections
  (let [v5-index {"ra" 0  "ri" 1  "ru" 2  "re" 3  "ro" 4 }
        v5-endings {:r   [ "ra"  "ri"  "ru" "re" "ro"]
                    :r-i [ "ra"  "ri"  "ru" "re" "ro"]
                    :s   [ "sa" "shi"  "su" "se" "so"]
                    :k   [ "ka"  "ki"  "ku" "ke" "ko"]
                    :g   [ "ga"  "gi"  "gu" "ge" "go"]
                    :b   [ "ba"  "bi"  "bu" "be" "bo"]
                    :m   [ "ma"  "mi"  "mu" "me" "mo"]
                    :n   [ "na"  "ni"  "nu" "ne" "no"]
                    :t   [ "ta" "chi" "tsu" "te" "to"]
                    :u   [ "wa"   "i"   "u"  "e"  "o"]
                    }
        
        
        reference     (:v5r inflections-base)
        template-keys (keys v5-templates)
        expand-template #(expand-template 
                           reference v5-index v5-endings v5-templates %)
        ]
    (apply merge inflections-base
      (map expand-template template-keys)))
  )  


(defn kannify [inflections] 
  (umap/mapvals 
    (fn [x] (cond
              (map? x)
              (umap/mapvals 
                (fn [y]
                  (cond
                    (sequential? y)
                    (map jpt/roma->kana-str y)
                    
                    (string? y)
                    (jpt/roma->kana-str y)
                    
                    true
                    ["just y" y]))
                x)
              
              (sequential? x)
              (map #(if (string? %) (jpt/roma->kana-str %) %) x)
              
              true
              x
              
              )) 
    inflections))


(def inflections-kana (kannify inflections))


(defn inflection-map->relation [[pos pos-map]]
  (mapcat (fn [[desc inflection-seq]]
         (map (fn [i] [pos desc i]) inflection-seq))
    pos-map))

(defn inflections->relation [inflections]
  (mapcat inflection-map->relation inflections))

(def inflections-blah
  (let [r (inflections->relation inflections)]
    (group-by #(nth % 2) r)))

(def inflections-blah-kana
  (let [r (map (fn [[pos desc i]]
                 [pos desc (jpt/roma->kana-str i)])
            (inflections->relation inflections))]
    (group-by #(nth % 2) r)))
;(umap/mapkeys t/roma->kana-str inflections-blah))
  
(def inflections-lookup-kana-1
  (umap/mapvals 
       (fn [v]
         (udl/add {}
           (apply concat
             (umap/mapvals second 
               (useq/map-by first
                 (mapcat
                   (fn [[desc inflection-seq]] 
                     (map (fn [i] [i desc]) inflection-seq))
                   v))))))
         inflections-kana))

(def inflections-lookup-kana
  (udl/add-unique {} (apply concat inflections-blah-kana)))

(defn lookup-and-group-by-pos [txt-seq]
  (group-by first 
    (apply concat 
      (vals (udl/lookup inflections-lookup-kana txt-seq)))))


(comment
  
(require '[utils.transliterate.jp :as jpt])
(require '[utils.dictionary.jp-inflect :as jpi] :reload)
(require '[clojure.pprint :as pp])
(require '[clojure.string :as s])
(require '[utils.x.core :as u])
(require '[utils.map :as umap])
(require '[utils.seq :as useq])
(require '[utils.x.swingdraw :as d]) 


(def p pp/pprint)

;(d/draw jpi/inflections-blah-kana)

   
(d/draw jpi/inflections-lookup-kana)
             
)