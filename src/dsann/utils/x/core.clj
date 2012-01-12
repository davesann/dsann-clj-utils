(ns dsann.utils.x.core
  (:refer-clojure :exclude [line-seq])
  (:require
    [clojure.string :as s]
    [clojure.java.io :as io]
    [clojure.pprint :as pp]

    [clojure.data.json :as json]
    [clojure.tools.logging :as log]
    
    )
  (:import java.lang.NumberFormatException)
  )

(def p pp/pprint)


(defn spp [data]
  (with-out-str (p data)))

(defn log 
  ([data] 
    (log/info (spp data))
    data)
  ([msg data]
    (log {:msg msg :data data})
    data))

(defn log-str 
  ([data] 
    (log/info (print-str data))
    data)
  ([msg data]
    (log-str {:msg msg :data data})
    data))

;; write files
(defn write-g [print-f fname data]
  (binding [*print-length* nil
            *print-level* nil]
   (with-open 
     [w (io/writer fname :encoding "UTF-8")] 
     (binding [*out* w] (print-f data)))))

(defn write-prn [fname data]
   (write-g prn fname data))
   
(defn write-pp [fname data]
  (write-g pp/pprint fname data))

(defn write-p [fname data]
  (write-g print fname data))

(defn write-json [fname data]
  (write-g json/pprint-json fname data))

;; read files
(defn line-seq [fname]
  (clojure.core/line-seq (io/make-reader fname nil)))