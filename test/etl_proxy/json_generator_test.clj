(ns etl-proxy.json_generator_test
  (:use clojure.test
        etl-proxy.json.generator
        [etl-proxy.graph define comparison]
        cheshire.core)
  (:require [clojure.java.io :as io]))

(org.apache.log4j.LogManager/resetConfiguration)
(org.apache.log4j.PropertyConfigurator/configure "resources/log4j.leintest.properties")

(deftest vertex-generalization-test
  (testing "Check following map rule with complex key-value vector reduction."
    (is (graph= (compose-json 2 [#{[1 "ROOT"] [2 ['(:a "A") '(:b "B") '(:c "C")]] [3 "LOWER"]} #{[1 2] [2 3]}])
                [#{[1 "ROOT"] [2 {:a "A", :b "B", :c "C"}] [3 "LOWER"]} #{[1 2] [2 3]}])))
  (testing " Follow vector rule."
    (is (graph= (compose-json 2 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}])
                (compose-json 3 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}])
                (compose-json 4 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}])
                [#{[1 "ROOT"] [2 ["A" "B" "C"]] [5 "LOWER"]} #{[1 2] [2 5]}])))
  (testing " Follow circuit rule."
    (are [id to] (graph= to (compose-json id [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}]))
         2 [#{[1 "ROOT"] [2 '(:a "A")] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [4 5] [6 7] [2 8] [5 8] [7 8]}]
         5 [#{[1 "ROOT"] [2 :a] [3 "A"] [4 '(:b "B")] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [6 7] [3 8] [4 8] [7 8]}]
         6 [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 '(:c "C")] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [3 8] [5 8] [6 8]}]))
  (testing "Check following terminal rule"
    (is (= (compose-json 1 [#{[1 "ROOT"]} #{}])
           [#{[1 "ROOT"]} #{}]))
    (is (= (compose-json 1 [#{[1 '("ROOT" "aaa")]} #{}])
           [#{[1 {"ROOT" "aaa"}]} #{}]))))

(deftest write-json-test
  (testing "Test json string generate process."
    ;; Follow logical rules during processing.
    (is (= (graph2json empty-graph) "{}"))
    ;; Short JSON expression convert check.
    (is (= {"root"
            {"a" 1,
             "b" 2,
             "c" 3,
             "d" ["DA" "DB" "DC"],
             "e" {"ea" 11, "eb" 12, "ec" 13}}
            }
           (parse-string
            (graph2json
             [#{[1 "root"]
                [2 "a"] [3 1]
                [4 "b"] [5 2]
                [6 "c"] [7 3]
                [8 "d"] [9 "DA"] [10 "DB"] [11 "DC"]
                [12 "e"]
                [13 "ea"] [14 11]
                [15 "eb"] [16 12]
                [17 "ec"] [18 13]}
              #{[1 2] [1 4] [1 6] [1 8] [1 12]
                [2 3] [4 5] [6 7]
                [8 9] [8 10] [8 11]
                [12 13] [12 15] [12 17]
                [13 14] [15 16] [17 18]}]))))))
