(ns dsann.utils.state.update
  (:require 
    [dsann.utils.x.core :as u]
    
    [dsann.utils.map :as umap]
    [dsann.utils.seq :as useq]
    [clojure.set :as cset]
    )
  )

;; bind *actor* if you want to flag who did the update.
(def ^:dynamic *actor* ::unspecified)
      
(defn update-in! [an-atom ks f & args]
  (let [alter-fn
        (fn [state]
          (with-meta
            (apply (partial update-in state ks f) args)
            {:update-path ks
             :action :update
             :update-by *actor*
             }))]         
    (swap! an-atom alter-fn)))

;; apply this for modified lists

(defn update-in2! [an-atom ks f & args]
  (let [alter-fn
        (fn [state]
          (let [v (get-in state ks)
                p (if (sequential? v)
                    (umap/mapvals second
                                  (group-by first (map-indexed (fn [i x] [x i]) v))))
                r (f v)
                updates (if p
                          (map (fn [x] (p x)) r))
                ]
            (with-meta
              (apply (partial update-in state ks f) args)
              {:update-path ks
               :action :update
               :seq-updates updates
               :update-by *actor*
               })))]         
  (swap! an-atom alter-fn)))

(defn differs-in? [m path v]
  (not (= (get-in m path) v)))

(defn swap-assoc-in [state ks v]
  (with-meta
    (assoc-in state ks v)
    {:update-path ks
     :action :set
     :update-by *actor*
     })) 

(defn swap-assoc-in? [state ks v]
  (if(differs-in? state ks v)
    (swap-assoc-in state ks v)
    state))

(defn assoc-in! [an-atom ks v]
  (swap! an-atom swap-assoc-in ks v))

(defn assoc-in?! [an-atom ks v]
  (when (differs-in? @an-atom ks v)
    (swap! an-atom swap-assoc-in ks v)))

(defn swap-dissoc-in [state ks]
  (with-meta
    (umap/dissoc-in state ks)
    {:update-path ks
     :action :dissoc
     :update-by *actor*
     }))         

(defn dissoc-in! [an-atom ks]
  (swap! an-atom swap-dissoc-in ks))

(defn filter-i
  ([pred? coll] (filter-i pred? coll 0 [] []))
  ([pred? coll idx removed result]
    (if-let [s (seq coll)]
      (let [f (first s) r (rest s)]
        (if (pred? f)
          (recur pred? r (inc idx) removed (cons f  result))
          (recur pred? r (inc idx) (cons idx removed) result)))
      {:result (reverse result)
       :removed-indices (set removed)}
      )))

(defn remove-i [pred? coll]
  (filter-i (complement pred?) coll))


;; note not returning actual removed - but the full index set
(defn remove-by-index-i [index-set a-seq]
  {:result (useq/remove-by-index index-set a-seq)
   :removed-indices index-set})

(defn filter-by-index-i [index-set a-seq]
  {:result (useq/filter-by-index index-set a-seq)
   :removed-indices index-set})

(defn swap-remove-in [state ks remove-fn pred?]
  (let [l (get-in state ks)
        {:keys [result removed-indices]} (remove-fn pred? l)]
    (with-meta
      (assoc-in state ks (vec result))
      {:update-path ks
       :action :list-remove
       :removed-indices removed-indices
       :update-by *actor*})))

(defn swap-append-in [state ks values]
  (let [l (get-in state ks [])]
    (with-meta
      (assoc-in state ks (apply (partial conj l) values))
      {:update-path ks
       :action :list-append  ;; assume that you have access to old value
       :appended values
       :update-by *actor*})))


(defn remove-in! [an-atom ks pred?]
  (swap! an-atom swap-remove-in ks remove-i pred?))

(defn filter-in! [an-atom ks pred?]
  (swap! an-atom swap-remove-in ks filter-i pred?))

(defn remove-by-index-in! [an-atom ks index-set]
  (swap! an-atom swap-remove-in ks remove-by-index-i index-set))

(defn filter-by-index-in! [an-atom ks index-set]
  (swap! an-atom swap-remove-in ks filter-by-index-i index-set))

(defn append-in! [an-atom ks & values]
  (swap! an-atom swap-append-in ks values))


; for reset use reset!
; if there is no meta data on change - assume the entire structure has updated







(defn index-map 
  "maps items to their index"
  ([a-seq] (index-map a-seq 0 {}))
  ([[i & r] idx result]
    (if (nil? i)
      (umap/mapvals reverse result)
      (let [new-result (update-in 
                         result [i] 
                         #(if (nil? %) (list idx) (cons idx %)))]
        (recur r (inc idx) new-result)))))
  
(defn map-moves-
  [coll idx imap result]
  (if (not (seq coll))
    {:new-list-changes result
     :old-list-deletes (mapcat second imap)}
    (let [[item & r] coll]
      (if-let [indexes (get imap item)]
        (let [[i & rindexes] indexes
              result (assoc result idx [(if (= i idx) :unchanged :move) i])
              imap   (assoc imap item rindexes)
              ]
          (recur r (inc idx) imap result))
        (let [result (assoc result idx [:insert item])]
          (recur r (inc idx) imap result))))))
  
(defn map-moves 
  "calculate the changes to get form seq1 to seq2"
  ([seq1 seq2] 
    (let [imap (index-map seq1)]
      (map-moves- seq2 0 imap {}))))


(comment
  (su/map-moves [ :A :B :C] [:C :D :A :F])
  (su/map-moves [:C :D :A :F] [ :A :B :C] )

(map-moves [1 2 3] [3 2 1])

(def l1
  (for [i (range 10000)]
    (rand-int 20)))

(def l2
  (for [i (range 10000)]
    (rand-int 20)))

(def moves (time (map-moves l1 l2)))
)



