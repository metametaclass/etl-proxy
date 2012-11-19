(defproject etl-proxy "0.1.0-SNAPSHOT"
  :description "Extract Transform Load database engine."
  :url "https://github.com/proofit404/etl-proxy"
  :license {:name "BSD 3-Clause License"
            :url "http://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [cheshire "4.0.2"]
                 [org.clojure/tools.logging "0.2.3"]
                 [log4j "1.2.15"
                  :exclusions [javax.mail/mail
                               javax.jms/jms
                               com.sun.jdmk/jmxtools
                               com.sun.jmx/jmxri]]]
  :plugins [[lein-marginalia "0.7.1"]
            [lein-dependencies "0.1.0"]]
  :min-lein-version "2.0.0")
