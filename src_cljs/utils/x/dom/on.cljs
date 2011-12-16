(ns utils.x.dom.on
  (:require
    [pinot.dom    :as pdom]
    
    [goog.dom     :as gdom]
    [goog.events  :as gevents]
    [goog.dom.Range :as grange]
    ;[goog.async.Throttle as gat]
    
    
    [utils.seq :as useq]
    
    [utils.x.core :as u]
    [utils.x.dom.range :as udr]
    [utils.x.dom.events :as ude]
    )
  ) 



(defn value-changed [value-fn change-fn start-events end-events elems]
  "
  value-fn     : extracts a value from an element
  change-fn    : called when the value changes
  start-events : event(s) that set the old value
  end-events   : event(s) that trigger a check for a change in value and call 
                 the change-fn
  "
  
  (doseq [el elems]
    (let [old-value (atom nil)
          start-fn (fn [e evt] (reset! old-value (value-fn e)))
          end-fn   (fn [e evt]
                     (let [new-value (value-fn e)]
                       (if (= @old-value new-value)
                         nil
                         (let [ov @old-value]
                           (reset! old-value new-value)
                           (change-fn e ov new-value)))))
          ]
      (doseq [se (useq/ensure-sequential start-events)]
        (ude/on el se start-fn))
               
      (doseq [ee (useq/ensure-sequential end-events)] 
        (ude/on el ee end-fn)))))


(defn text-select [f elements]
  "on text selection call (f element event range)"
  (ude/on elements
         :mouseup
         (fn [e evt] 
           (let [r (grange/createFromWindow)]
             (if-not (udr/empty-range? r)
               (f e evt r))))))


(defn get-time []
  (. (js/Date.) (getTime)))


(defn throttle-to-last-fn [f min-delay]
  (let [called (atom nil)
        held   (atom nil)]
    (fn [& args]
      
      (if @called
        (reset! held args)       
        (do
          (reset! called 1)
          (apply f args) 
          (js/setTimeout 
            (fn []
              (do 
                (if-let [args @held]
                  (do
                    (apply f args)
                    (reset! held nil)))
                (reset! called nil)))
            min-delay)
          )))))
  

(defn text-select-move [notify-complete notify-intermediate]
  "on text selection call (f element event range)"
  (let [body (first (pdom/query "body"))
        listeners (atom nil)
        
        notify-helper (fn [f e evt]
                        (if-let [r (grange/createFromWindow)]
                          (if-not (udr/empty-range? r)
                            (f e evt r))))
        
        notify-and-remove (fn [e evt]
                            (do 
                              (notify-helper notify-complete e evt)
                              (let [l @listeners]
                                (reset! listeners nil)
                                (ude/remove-listeners l))))
        ]
    (ude/on body
           :mousedown
           (fn [e evt]
             (let [l1 (if notify-intermediate
                        (ude/on body :mousemove 
                                (throttle-to-last-fn 
                                  (partial notify-helper notify-intermediate) 
                                  30))
                        nil)
                   l2 (ude/on body :mouseup notify-and-remove)]
               (reset! listeners (concat l1 l2)))))))
             
             




