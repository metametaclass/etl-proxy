(ns etl-proxy.json_parser_test
  (:use clojure.test
        etl-proxy.json.parser
        etl-proxy.graph.transform
        etl-proxy.graph.comparison
        etl-proxy.graph.crud))

(deftest vertex-simplification-test
  (testing "Non recursive single vertex simplification."
    ;; Simple vertex mast be unchanged.
    (is (graph= (transform-vertex 1 [#{[1 "ABC"]} #{}])
           [#{[1 "ABC"]} #{}]))
    ;; Vector must be insert as sequence of its elements.
    (is (graph= (transform-vertex 2 [#{[1 "ROOT"] [2 ["A" "B" "C"]] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 "A"] [5 "B"] [6 "C"] [3 "LOWER"]}
            #{[1 4] [1 5] [1 6] [4 3] [5 3] [6 3]}]))
    ;; Map must be insert as sequence of its simplified versions.
    (is (graph= (transform-vertex 2 [#{[1 "ROOT"] [2 {:a "A" :b "B" :c "C"}] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 ['(:a "A") '(:c "C") '(:b "B")]] [3 "LOWER"]}
            #{[1 4] [4 3]}]))
    ;; List must be insert as series of its elements with parent child relation.
    (is (graph= (transform-vertex 2 [#{[1 "ROOT"] [2 '("A" "B" "C")] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 "A"] [5 "B"] [6 "C"] [3 "LOWER"]}
            #{[1 4] [4 5] [5 6] [6 3]}]))))

(deftest read-json-file
  (testing "Test simple json string parse process."
    ;; Follow logical rules during processing.
    (is (= (json2graph "{}") empty-graph))
    ;; JSON string converter check.
    (is (graph= (json2graph
                 "{ 
                    \"root\" : {
                      \"a\": 1,
                      \"b\": 2,
                      \"c\": 3,
                      \"d\": [
                        \"DA\",
                        \"DB\",
                        \"DC\"
                      ],
                      \"e\": {
                        \"ea\" : 11,
                        \"eb\" : 12,
                        \"ec\" : 13
                      }
                    }
                  }")
                ;; Result graph as it is.
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
                   [13 14] [15 16] [17 18]}]))))
