(ns etl-proxy.json_generator_test
  (:use clojure.test
        etl-proxy.json.generator
        [etl-proxy.graph comparison]))

(deftest vertex-generalization-test
  (testing "Check for following vertex generalization rules."
    ;; Follow vector rule.
    (is (graph= (compose-json 2 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}])
                (compose-json 3 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}])
                (compose-json 4 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}])
                [#{[1 "ROOT"] [2 ["A" "B" "C"]] [5 "LOWER"]} #{[1 2] [2 5]}]))
    ;; Follow circuit rule.
    (are [id of to] (graph= (compose-json id of) to)
         ;; -------
         2 [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}]
           [#{[1 "ROOT"] [2 '(:a "A")] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]}  #{[1 2] [1 4] [1 6] [4 5] [6 7] [2 8] [5 8] [7 8]}]
         ;; -------
         5 [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}]
           [#{[1 "ROOT"] [2 :a] [3 "A"] [4 '(:b "B")] [6 :c] [7 "C"] [8 "LOWER"]}  #{[1 2] [1 4] [1 6] [2 3] [6 7] [3 8] [4 8] [7 8]}]
         ;; -------
         6 [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}]
           [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 '(:c "C")] [8 "LOWER"]}  #{[1 2] [1 4] [1 6] [2 3] [4 5] [3 8] [5 8] [6 8]}])
    ;; Terminal rule with complex key-value vector reduction.
    (is (= (compose-json 1 [#{[1 "ROOT"]} #{}])
           [#{[1 "ROOT"]} #{}]))
    (is (graph= (compose-json 1 [#{[1 "ROOT"] [2 ['(:a "A") '(:b "B") '(:c "C")]] [3 "LOWER"]}
                                 #{[1 2] [2 3]}])
                [#{[1 "ROOT"] [2 {:a "A", :b "B", :c "C"}] [3 "LOWER"]}
                 #{[1 2] [2 3]}]))))

(deftest write-json-test)
