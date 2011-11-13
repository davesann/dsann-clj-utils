(defproject dsann/utils "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
 :omit-default-repositories true
  :repositories {"releases"  "http://artifactory/artifactory/libs-release"
                 "snapshots" "http://artifactory/artifactory/libs-snapshot"}
                 
  :dependencies     [[org.clojure/clojure "1.3.0"]
                     
                     ; for xml
                     [org.clojure/clojure-contrib "1.2.0"]
                     [enlive "1.0.0"]
                     
                     ; for html temporary
                     [hiccup "0.3.6"]
                     
                     
                     [org.clojure/tools.logging "0.2.0"]
                     [org.clojure/data.json "0.1.1"]
                     
                     [seesaw "1.1.0"]
                      
                     ]
  :dev-dependencies [[lein-eclipse "1.0.0"]]
)

