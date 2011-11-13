(ns utils.dictionary.bidict
  (:require [utils.x.core :as u]
            [utils.dictionary.monodict :as mdict]
            ))

(defn make-dict [] {})

(defn add-word [d word trans]
  (let [forward  (mdict/add-word (:forward d) word trans)
        backward (mdict/add-word (:backward d) trans word)]
  (assoc d 
         :forward forward
         :backward backward)))

(defn add-words [d [word trans & others]]
  (let [new-d (add-word d word trans)]
    (if (nil? others)
      new-d
      (recur new-d others))))
  
(defn lookup-by-freq 
  ([d word]
    (lookup-by-freq :forward d word))
  ([dir d word]
    (:list-by-count (get (dir d) word))))

(defn lookup-first 
  ([d word]
    (lookup-first :forward d word))
  ([dir d word]
    (first 
      (first (lookup-by-freq dir d word)))))


(defn test-add []
  (let [d (add-words {}
                     ["hello" "bonjour"
                      "hello" "bonjour"
                      "hello" "bonjour1"
                      "hello" "bonjour1"
                      "hello" "bonjour1"
                      "hello" "salut"
                      "good" "bon"])]
    (u/qclojure.tools.logging d)
    (lookup-first d "hello")))
