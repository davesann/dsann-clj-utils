(defproject dsann/utils "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :omit-default-repositories true
  :repositories {"releases"  "http://artifactory/artifactory/libs-release"
                 "snapshots" "http://artifactory/artifactory/libs-snapshot"}
                 
  :dependencies     [[org.clojure/clojure "1.3.0"]
                     
                     [org.clojure/tools.logging "0.2.0"]
                     [org.clojure/data.json "0.1.1"]
                                           
                     ]
  :dev-dependencies [[lein-eclipse "1.0.0"]]
  
  :jvm-opts ["-DentityExpansionLimit=10000000"]
)

