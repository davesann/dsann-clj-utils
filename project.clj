(defproject dsann/dsann-clj-utils "0.1.0"
  :description "general utilities"
                 
  :omit-default-repositories true
  :repositories {"global-releases"  "http://artifactory/artifactory/libs-release"
                 "global-snapshots" "http://artifactory/artifactory/libs-snapshot"
                 "snapshots" "http://artifactory/artifactory/libs-snapshot-local"
                 "releases"  "http://artifactory/artifactory/libs-release-local"
                 }
  
  :dependencies     [[org.clojure/clojure "1.3.0"]
                     
                     [org.clojure/tools.logging "0.2.0"]
                     [org.clojure/data.json "0.1.1"]
                                           
                     ]
  :dev-dependencies [[lein-eclipse "1.0.0"]]
  )

