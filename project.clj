(defproject lumo "0.1.0-SNAPSHOT"
  :description "Time of day based controller for a light stick"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-time "0.13.0"]
                 [com.evocomputing/colors "1.0.3"]
                 [clj-opc "0.1.1"]
                 [environ "1.1.0"]]
  :plugins [[lein-environ "1.1.0"]]
  :profiles {:uberjar {:aot :all}}
  :main lumo.core)
