(ns dsann.utils.seq-table)

;; Alpha

(defn tabulate-overlap-acc 
  ([indexed-seq held iresult fresult]
    (if (empty? indexed-seq)
      (if (empty? held)
        (conj fresult iresult)
        (recur held [] [] (conj fresult iresult)))
      (let [[item & remainder] indexed-seq
            [[s e] v] item
            [n-held new-indexed-seq] (split-with (fn [[[s1 _] _]] (> e s1)) remainder)
           
            _x (doall n-held) ; obscure bug in clojurescript
            
            ]
        (recur new-indexed-seq (concat held n-held) (conj iresult v) fresult)
        ))))

(defn tabulate-overlap
  "given an indexed sequence [[[from to] value] [[from to] value] ...]
creates a list of lists where no index spans overlap in each row.

from values are inclusive 
to values are exclusive
"
  ([indexed-seq] (tabulate-overlap-acc indexed-seq [] [] [])))
  


