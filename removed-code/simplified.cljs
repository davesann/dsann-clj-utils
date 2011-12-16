
(def partition-same (partial partition-by identity))


;; original code for the above.
;; http://blog.fogus.me/2011/03/09/recursion-is-a-low-level-operation/
;; gave the above version.
  
(defn partition-same-x
  
  ([a-list]
    (partition-same a-list (first a-list) nil nil))
  ([a-list i result results]
    (if (empty? a-list)
      (reverse (cons result results))
      (let [i? (first a-list)
            r  (rest a-list)
            ]
        (if (= i i?)
          (recur r i  (cons i result)  results             )
          (recur r i? (list i?)        (cons result results)))))))

(defn partition-with [f a-seq]
  (group-by f a-seq))

;; simplified above - values have to be extracted
(defn partition-with-x 
  ([f a-seq] (partition-with f a-seq [] []))
  ([f [v & a-seq] yes-list no-list]
    (if (and (nil? a-seq) (nil? v))
      [yes-list no-list]
      (if-let [r (f v)]
        (recur f a-seq (conj yes-list v) no-list)
        (recur f a-seq yes-list (conj no-list v))))))

      