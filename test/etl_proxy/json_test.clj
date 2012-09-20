(ns etl-proxy.json_test
  (:use clojure.test
        etl-proxy.json
        cheshire.core))

(deftest read-json-file
  (testing "Check correct reading json file into graph structure."
    (is (= (parse-string (slurp (str (. (java.io.File. ".") getCanonicalPath) "/test/etl_proxy/message_example.json")) true)))))
