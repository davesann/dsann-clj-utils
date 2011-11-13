(ns utils.html
  (:require
    [clojure.string :as string]
    [clojure.pprint :as pp]
    [clojure.tools.logging :as log]
    
    [utils.core :as u]
    )
  )


(defn keyword->title [kw]
  (string/capitalize 
    (string/join " " (s/split (name kw) #"-"))))
        
(defn data->pre [data]
  [:pre (with-out-str (pp/pprint data ))])

(defn names->checkboxes [prefix names params]
  (map (fn [n]
         (let [k (keyword (str prefix n))
               check-m (if (nil? (params k))
                         {}
                         {:checked "true"})
               attr (merge check-m {:type :checkbox 
                                    :name  k
                                    :value n})]
           [:input attr n]))
       names))
  
(defn submit-button [value]
  [:input.button {:type "submit" :name "submit-value" :value value}])


(defn keysplit [k]
 (let [raw-names (string/split (name k) #"[.]")]
   (map (fn [rn]
          (let [sub-names (string/split rn #"-")]
            (string/join " " (map string/capitalize sub-names))))
        raw-names)))
   
(defn col-names [table-config]
  (let [col-keys (:cols table-config)
        col-name-lookup (:col-names table-config)
        names (map (fn [k]
                     (if-let [col-name (k col-name-lookup)]
                       [col-name]
                       (keysplit k)))
                   col-keys)
        ]
    {:max-count (apply max (map count names))
     :names names  
     }))

(defn col-fns [table-config]
  (let [col-keys       (:cols table-config)
        lookup         (:col-fns table-config)
        default-col-fn (or (:default-col-fn table-config)
                           (fn [k d] (u/hget d k)))
        ]
    (map (fn [k]
           (if-let [col-fn (k lookup)]
             col-fn
             default-col-fn))
         col-keys)))

(defn normalise-table-config [table-config]
  "adds in default col names and col-fns if necessary"
  (assoc table-config
         :col-names (col-names table-config)
         :col-fns   (col-fns table-config)
         :attr      (or (:attr table-config {}))
         ))

(defn list->tr [a-list]
  [:tr (map (fn [x] 
              (if (map? x)
                [:td (:attr x) (:content x)]
                [:td {} x]))
            a-list)])

(defn list->thr-g [a-list]
  (let [ps (u/partition-same a-list)]
    [:tr (map (fn [p] 
                (let [c (count p)
                      fp (first p)
                      attr (if (> c 1)
                             {:colspan (count p)
                              :class "colspan"})
                      ]
                  [:th attr (first p)])) 
              ps)
     ]))


(defn get-prefixed-values [prefix params]
  (let [c (count prefix)]
    (remove nil?
            (map (fn [k] 
                   (let [n (name k)]
                     (if (.startsWith n prefix)
                       (.substring n c))))
                 (keys params)))))

(defn list-of-lists->table 
  ([list-of-lists] (list-of-lists->table {} list-of-lists))
  ([attr list-of-lists]
    [:table attr (map list->tr list-of-lists)]))

(defn list->table [col-count & ls]
  (let [l (apply concat ls)
        pc (min col-count (count (take col-count l)))]
    (list-of-lists->table  
      (partition pc pc (cycle " ") l))))

(defn transpose [limit list-of-lists ]  
  (let [firsts (map first list-of-lists)
        rests  (map rest list-of-lists)]
    (if (<= limit 1)
      (list firsts)
      (cons firsts (transpose (dec limit) rests)))))

(defn tconfig->thead [table-config]
  (let [col-names     (:col-names table-config)
        num-rows      (:max-count col-names)]
    [:thead 
     (let [t (transpose (:max-count col-names) (:names col-names))]
       (map list->thr-g t))
     ]))

(defn map->table-row [a-map table-config]
  (let [col-keys     (:cols table-config)
        col-fns      (:col-fns table-config)
        default-attr (or (:default-attr table-config) {})
        ]
    (list->tr 
      (map (fn [f k] 
             (let [r (f k a-map)]
               (if (and (not (map? r)) default-attr)
                 {:attr default-attr :content r}
                 r)))
           col-fns
           col-keys
           ))))
           
(defn maplist->table-rows [map-list table-config]
  (map #(map->table-row % table-config) 
       map-list))

(defn maplist->table
  ([map-list] 
    (maplist->table map-list nil))
  ([map-list table-config]
    (let [tconfig (normalise-table-config 
                    (or table-config
                        {:cols (keys (first map-list))}))]
      [:table (:attr tconfig)
       (tconfig->thead tconfig)
       [:tbody (maplist->table-rows map-list tconfig)]
       ])))


