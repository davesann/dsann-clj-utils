(ns utils.x.dom.events
  (:require
    [clojure.string :as s]
    
    [pinot.dom    :as pdom]
    [pinot.events :as pe]
    [pinot.util.clj :as pclj]
    [pinot.util.js :as pjs]
    
    [goog.events :as gevents]

    [utils.x.core  :as u]
    )
  ) 


(defn log-handler [element event]
  (do
    (u/log element)
    (u/log event)))

(def log log-handler)

(defn ctrl-key? [event]
  (.ctrlKey event))

(defn right-button? [event]
  (= 2 (.button event)))



(defn remove-listeners [listener-ids]
  (doseq [ id listener-ids ] 
    (gevents/unlistenByKey id)))


;; modified from pinot events 
(defn on [elem event func]
  (let [ev-name (s/upper-case (name event))
        event (aget gevents/EventType ev-name)
        body-elem (pe/get-body)]
    (doall (map
             (fn [el]
               (let [parsed (pe/->target el)]
                 (gevents/listen body-elem
                                 event
                                 (pe/make-listener func parsed))))
             (pclj/->coll elem)))
    ))


(defn on-l [elems event-type listener-handler]
  "adds a handler that receives the
    element  
    event
    listener id of the attached listener (to enable removal)"
  (let [add-fn (fn [e]
                 (let [listener-ids (atom nil)
                       h (fn [elem event]
                           (listener-handler elem event @listener-ids))
                       ids  (on e event-type h)
                       ]
                   (reset! listener-ids ids)))
        ]
    (if (sequential? elems)
      (map add-fn elems)
      (add-fn elems))))
      
(defn once-on [elem event-type handler]
  (on-l elem event-type
        (fn [elem event listener-ids]
          (remove-listeners listener-ids)
          (handler elem event))))

(defn ntimes-on [elem event-type handler n]
  (let [c (atom 1)]
    (on-l elem event-type
          (fn [elem event listener-ids]
            (if (>= @c n)
              (remove-listeners listener-ids)
              (swap! c inc))
            (handler elem event)))))

