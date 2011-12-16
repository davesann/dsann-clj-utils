(ns utils.x.dom.range
  (:require
    [pinot.dom :as pdom]
    [goog.dom :as gdom]
    [utils.x.dom :as udom]
    
    [goog.dom.Range :as grange]
    [goog.dom.TextRangeIterator :as grit]
    [goog.dom.annotate :as gann]
    
    [utils.x.core :as u]
    [utils.seq :as useq]
    [utils.html :as uh]
    )
  ) 

(defn ctrl-key? [event]
  (.ctrlKey event))

(defn right-button? [event]
  (= 2 (.button event)))

(defn empty-range? [rng]
  (. rng (isCollapsed)))

(defn text [rng]
  (. rng (getText)))

(defn no-space? [txt]
  (= -1 (. txt (indexOf " "))))

(defn range-start [rng]
  (. rng (getStartOffset)))

(defn range-end [rng]
  (. rng (getEndOffset)))

(defn range-start-node [rng]
  (. rng (getStartNode)))

(defn range-end-node [rng]
  (. rng (getEndNode)))


(defn range->range-iterator [rng]
  (let [start-offset (. rng (getStartOffset))
        start-node   (. rng (getStartNode))
        end-offset   (. rng (getEndOffset))
        end-node     (. rng (getEndNode))]
    (goog.dom.TextRangeIterator. start-node start-offset 
                                 end-node end-offset)))

(defn g-iterator->seq 
  ([it] (g-iterator->seq it (list)))
  ([it result]
    (if (. it (isLast))
      (if (vector? result) result (reverse result))
      (let [n (. it (next))
            n-result (conj result n)]
        (recur it n-result)))))

(defn g-iterator->list [it]
  (g-iterator->seq it (list)))

(defn g-iterator->vec [it]
  (g-iterator->seq it []))

(defn map-text-nodes [node f]
  (let [nodes (pdom/nodelist->coll (.childNodes node))]
    (map (fn [n]
           (if (text-node? n) 
             (f n)
             (map-text-nodes n f) 
             ))
         nodes)))
