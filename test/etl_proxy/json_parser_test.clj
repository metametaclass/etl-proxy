(ns etl-proxy.json_parser_test
  (:use clojure.test
        etl-proxy.json.parser
        [etl-proxy.graph define comparison]))

(deftest vertex-simplification-test
  (testing "Non recursive single vertex simplification."
    ;; Simple vertex mast be unchanged.
    (is (graph= (decompose-json "ABC" [#{[1 "ABC"]} #{}])
           [#{[1 "ABC"]} #{}]))
    ;; Vector must be insert as sequence of its elements.
    (is (graph= (decompose-json ["A" "B" "C"] [#{[1 "ROOT"] [2 ["A" "B" "C"]] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 "A"] [5 "B"] [6 "C"] [3 "LOWER"]}
            #{[1 4] [1 5] [1 6] [4 3] [5 3] [6 3]}]))
    ;; Map must be insert as sequence of its simplified versions.
    (is (graph= (decompose-json {:a "A" :b "B" :c "C"} [#{[1 "ROOT"] [2 {:a "A" :b "B" :c "C"}] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 ['(:a "A") '(:c "C") '(:b "B")]] [3 "LOWER"]}
            #{[1 4] [4 3]}]))
    ;; List must be insert as series of its elements with parent child relation.
    (is (graph= (decompose-json '("A" "B" "C") [#{[1 "ROOT"] [2 '("A" "B" "C")] [3 "LOWER"]} #{[1 2] [2 3]}])
           [#{[1 "ROOT"] [4 "A"] [5 "B"] [6 "C"] [3 "LOWER"]}
            #{[1 4] [4 5] [5 6] [6 3]}]))))

(deftest read-json-file
  (testing "Test simple json string parse process."
    ;; Follow logical rules during processing.
    (is (= (json2graph "{}") empty-graph))
    ;; Short JSON expression convert check.
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
                   [13 14] [15 16] [17 18]}]))
    ;; Check for working with longer expressions. This is type conversion problem when
    ;; PersistentArrayMap became PersistentHashMap in the maps which contain more than eight
    ;; elements.
    (is (graph= (json2graph "{ 
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
                        \"ec\" : 13,
                        \"ed\" : 14,
                        \"ee\" : 15,
                        \"ef\" : 16,
                        \"eg\" : 17,
                        \"eh\" : 18,
                        \"ei\" : 19,
                        \"ej\" : 20,
                        \"ek\" : 21,
                        \"el\" : 22,
                        \"em\" : 23,
                        \"en\" : 24,
                        \"eo\" : 25
                      }
                    }
                  }")
                [#{[69 "ek"] [67 "ej"] [65 "ei"] [63 "eh"] [61 "eg"] [59 "ef"] [22 "DA"] [23 "DB"] [24 "DC"] [57 "ee"] [55 "ed"] [48 25] [53 "ec"] [46 24] [51 "eb"] [44 23] [49 "ea"] [42 22] [70 21] [68 20] [20 "e"] [66 19] [17 3] [18 "d"] [64 18] [15 2] [16 "c"] [62 17] [13 1] [14 "b"] [60 16] [12 "a"] [58 15] [56 14] [54 13] [52 12] [50 11] [47 "eo"] [45 "en"] [43 "em"] [4 "root"] [41 "el"]}
                 #{[20 51] [67 68] [69 70] [41 42] [43 44] [12 13] [45 46] [14 15] [47 48] [16 17] [49 50] [51 52] [20 53] [53 54] [55 56] [57 58] [59 60] [61 62] [63 64] [65 66] [20 55] [18 22] [18 23] [20 57] [18 24] [20 59] [4 12] [20 61] [4 14] [20 63] [4 16] [20 65] [4 18] [20 67] [4 20] [20 69] [20 41] [20 43] [20 45] [20 47] [20 49]}]))))
