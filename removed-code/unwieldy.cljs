
(defn map-n 
  "apply f to the first item in the list and then every n items thereafter"
  ([f n a-seq] (map-n f n a-seq []))
  ([f n [item & a-seq] result]
    (if (and (nil? a-seq) (nil? item))
      result
      (let [r (f item)
            [skip remainder] (split-at (dec n) a-seq)]
        (if (empty? skip)
          (recur f n remainder (conj result r))
          (recur f n remainder (apply conj (conj result r) skip)))))))