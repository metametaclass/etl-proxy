(ns etl-proxy.atest)

(org.apache.log4j.LogManager/resetConfiguration)
(org.apache.log4j.PropertyConfigurator/configure "resources/log4j.leintest.properties")
(println "Leiningen: Successful setup log4j to the DEBUG level.")
