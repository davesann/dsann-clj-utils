(ns utils.annotation-fns
   (:require 
    [utils.seq :as useq]
    [utils.map :as umap]
    [utils.x.core :as u]
    )
   )


(defn partition->annotations-acc
  [count-fn [p & others] current-idx result]
  (if (nil? p)
    result
    (let [m (:match p)
          c (count-fn m)
          s current-idx
          e (+ current-idx c)]
      (recur count-fn others e 
             (conj result {{:start s :end e} (assoc p :len c)})))))

(defn partition->annotations 
  "take a partitioned sequence and turn it into a list of annotations
[{:start s :end e} {:val item :len (count item)}]

start and endi index into the original sequence
"
  [count-fn p] (partition->annotations-acc count-fn p 0 []))
  
(defn partition-annotate [count-fn partition-fn a-seq]
  (let [p (partition-fn a-seq)
        annotations (partition->annotations count-fn p)]
    {:context a-seq
     :annotations (apply merge annotations)
     }))

(defn annotation-start [annotation]
  (:start (first annotation)))

(defn annotation-end [annotation]
  (:end (first annotation)))

(defn annotation-length [annotation]
  (let [a (first annotation)]
    (- (:end a) (:start a))))

(defn annotation-range [annotation]
  (let [a (first annotation)]
    [(:start a) (:end a)]))

(defn annotation-value [annotation]
  (second annotation))

(defn sort-annotations-range [annotations]
  (let [cmp-fn (useq/r-compare-fn ffirst #(- (second (first %)) (ffirst %))) 
        values (sort cmp-fn (map (fn [a] [(annotation-range a) a]) annotations))]
    values))

(defn tabulate-annotations [annotations]
  "takes a set of text annotations and tabulates 
them so that no annotation overalps on any line

annotations is a map
{{:start s :end e} something}

The output is a list of lists of no-overlapping annotations
"
  (let [cmp-fn (useq/r-compare-fn ffirst #(- (second (first %)) (ffirst %))) 
        values (sort cmp-fn (map (fn [a] [(annotation-range a) a]) annotations))]
    (useq/tabulate-overlap values)))



;; chunk annotations

(defn longest-annotation-at [idx annotations-table]
  (if-let [fsts (useq/map- 
                  #(or (nil? %) (not= idx (annotation-start %)))
                  first 
                  annotations-table)]
    (let [[fst-a & rst-a] fsts]  
          (reduce (fn [best a]
                    (let [l1 (annotation-length best)
                          l2 (annotation-length a)]
                      (if (> l2 l1)
                        a
                        best)))
                  fst-a
                  rst-a))))

(defn- pad-empty-row [la l]
  (if (empty? l)
    [[{:start (annotation-start la) :end (annotation-end la)} 
      {:chunk-padding true :tag :space :match "\u00a0" :len (:len (annotation-value la))}]]
    l))

(defn next-chunk
  ([idx annotation-table]
    (if-let [la (longest-annotation-at idx annotation-table)]
      (let [lae (annotation-end la)
            s (map #(split-with (fn [a] (<= (annotation-end a) lae)) %)
                   annotation-table)]
        [la 
         (map #(pad-empty-row la (first %)) s) 
         (map second s)]))))

(defn chunk-annotation-table-acc [annotation-table next-index result]
  (if-let [nc (next-chunk next-index annotation-table)]
    (let [[longest-annotation table-chunk new-annotation-table] nc
          start-col (annotation-start longest-annotation)
          end-col   (annotation-end   longest-annotation)
          col-span  (- end-col start-col)]
      (recur new-annotation-table end-col (conj result [start-col col-span table-chunk])))
    result
    ))  

(defn chunk-annotation-table
  [annotation-table] (chunk-annotation-table-acc annotation-table 0 []))
      
