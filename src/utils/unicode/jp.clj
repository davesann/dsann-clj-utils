(ns utils.unicode.jp
  (:require [utils.logic :as ul]
    )
  )
  
;; code numbers from 
;;  http://www.rikai.com/library/kanjitables/kanji_codes.unicode.shtml
  
(defn hiragana? [c]
  (ul/between=? 0x3040 0x309F (int c)))

(defn katakana? [c]
  (ul/between=? 0x30A0 0x30FF (int c)))

(defn kanji? [c]
  (ul/between=? 0x4E00 0x9FAF (int c)))

(defn char-class [c]
  (cond
    (hiragana? c) :hiragana
    (katakana? c) :katakana
    (kanji? c)    :kanji
    true          :other))

