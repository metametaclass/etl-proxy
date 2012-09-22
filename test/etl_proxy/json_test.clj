(ns etl-proxy.json_test
  (:use clojure.test
        etl-proxy.json))

(deftest body-simplification-test
  (testing "Check following the simplification rules set."
    (is (= (simplify-body {:a 1 :b 2 :c 3 :d ["DA" "DB" "DC"] :e {:ea 11 :eb 12 :ec 13}})
           (vector '(:a 1)
                   '(:c 3)
                   '(:b 2)
                   '(:d ["DA" "DB" "DC"])
                   '(:e {:ea 11, :eb 12, :ec 13}))))
    (is (= (simplify-body "ABC") "ABC"))))

(deftest body-in-graph-simplification-test
  (testing "Check simplification of single vertex in the vector manner."
    (is (= (vector-simplification 2 ["A" "B" "C"] [#{[1 "ROOT"] [2 ["A" "B" "C"]] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [2 ["A" "B" "C"]] [3 "LOWER"] [4 "A"] [5 "B"] [6 "C"]}
            #{[1 2] [2 3] [2 4] [2 5] [2 6] [4 3] [5 3] [6 3]}])))
  (testing "Check simplification of single vertex in the map manner."
    (is (= (map-simplification 2 {:a "A" :b "B" :c "C"} [#{[1 "ROOT"] [2 {:a "A" :b "B" :c "C"}] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [2 {:a "A" :b "B" :c "C"}] [4 '(:a "A")] [6 '(:b "B")] [5 '(:c "C")] [3 "LOWER"]}
            #{[1 2] [2 3] [2 4] [2 5] [2 6] [4 3] [5 3] [6 3]}])))
  (testing "Check simplification of single vertex in the list manner."
    (is (= (list-simplification 2 '("A" "B" "C") [#{[1 "ROOT"] [2 '("A" "B" "C")] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [2 '("A" "B" "C")] [3 "LOWER"] [4 "A"] [5 "B"] [6 "C"]}
            #{[1 2] [2 3] [2 4] [4 5] [5 6] [6 3]}]))))

(deftest vertex-simplification-test
  (testing "Non recursive single vertex simplification."
    ;; Simple vertex mast be unchanged.
    (is (= (simplify-vertex 1 [#{[1 "ABC"]} #{}])
           [#{[1 "ABC"]} #{}]))
    ;; Vector must be insert as sequence of its elements.
    (is (= (simplify-vertex 2 [#{[1 "ROOT"] [2 ["A" "B" "C"]] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 "A"] [5 "B"] [6 "C"] [3 "LOWER"]}
            #{[1 4] [1 5] [1 6] [4 3] [5 3] [6 3]}]))
    ;; Map must be insert as sequence of its simplified versions.
    (is (= (simplify-vertex 2 [#{[1 "ROOT"] [2 {:a "A" :b "B" :c "C"}] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 '(:a "A")] [6 '(:b "B")] [5 '(:c "C")] [3 "LOWER"]}
            #{[1 4] [1 5] [1 6] [4 3] [5 3] [6 3]}]))
    ;; List must be insert as series of its elements with parent child relation.
    (is (= (simplify-vertex 2 [#{[1 "ROOT"] [2 '("A" "B" "C")] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 "A"] [5 "B"] [6 "C"] [3 "LOWER"]}
            #{[1 4] [4 5] [5 6] [6 3]}]))))

(deftest read-json-file
  (testing "Test simple json string parse process."
    (is (= (json2graph
            ;; JSON string as it is.
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
           [#{
              [3 "root"]
              [10 "a"] [11 1]
              [12 "b"] [13 2]
              [14 "c"] [15 3]
              [18 "d"] [23 "DA"] [24 "DB"] [25 "DC"]
              [16 "e"] [26 "ea"] [27 11]
                       [28 "eb"] [29 12]
                       [30 "ec"] [31 13]
              }
            #{[10 11][12 13][14 15][26 27][28 29][30 31][18 23][18 24][3 10][18 25][3 12][16 26][3 14][16 28][3 16][16 30][3 18]}]))))
