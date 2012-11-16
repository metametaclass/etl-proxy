(ns etl-proxy.graph_comparison_test
  (:use clojure.test
        etl-proxy.graph.comparison))

(deftest bodies-bind-test
  (testing "Verify correct binds search between two vertices in graph."
    (are [x y] (bodies-bound? x y [#{[1 "A"] [2 "B"] [5 "C"]} #{[2 1] [1 5]}])
         "A" "B"
         "C" "A")
    (is (not (bodies-bound? "B" "C" [#{[1 "A"] [2 "B"] [5 "C"]} #{[2 1] [1 5]}])))))

(deftest adjacency-map-test
  (testing "Building correct adjacency map for graph."
    (is (= (adjacency-map [#{[1 "A"] [2 "B"] [5 "C"]} #{[2 1] [1 5]}])
           {"A" #{"C"},
            "B" #{"A"},
            "C" #{}}))))

(deftest graph-equality-test
  (testing "Check compliance with graph equality principle."
    (is (graph=
         [#{[1 "A"] [2 "B"] [3 "C"] [4 "D"] [5 "E"]}
          #{[1 2] [2 3] [4 5] [4 1]}]
         [#{[11 "A"] [22 "B"] [33 "C"] [44 "D"] [55 "E"]}
          #{[11 22] [22 33] [44 55] [44 11]}]
         [#{[111 "A"] [222 "B"] [333 "C"] [444 "D"] [555 "E"]}
          #{[111 222] [222 333] [444 555] [444 111]}]))))

(deftest level-topology-test
  (testing "Check level topology detection."
    (is (level-topology? 2 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}]))
    (is (level-topology? 2 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER-1"] [6 "LOWER-2"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5] [2 6] [3 6] [4 6]}]))
    (is (not (level-topology? 2 [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [2 3] [3 5] [1 4] [4 5]}]))))
  (testing "Check getting level topology from graph."
    (are [x ser] (= ser (get-level x [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}]))
         2 [2 3 4]
         3 [2 3 4]
         4 [2 3 4])
    (are [x ser] (= ser (get-level x [#{[1 "ROOT"] [2 "A"] [3 "B"] [4 "C"] [5 "LOWER"]} #{[1 2] [1 3] [1 4] [2 5] [3 5] [4 5]}]))
         1 nil
         5 nil))
  (testing "Testing complex level with elements which must be excluded from it."
    ;; There is no level below because vertices with 8 and 12 ids doesn't appropriate to this rule.
    (is (= nil ;; Not [2 3 8 12 19]
           (get-level 2 [#{[1 "root"]
                           [19 '("a" 1)]
                           [2  '("b" 2)]
                           [3  '("c" 3)]
                           [8 "d"] [4 ["DA" "DB" "DC"]]
                           [12 "e"] [5 '("ea" 11)] [6 '("eb" 12)] [7 '("ec" 13)]}
                         #{[1 2] [1 3] [1 8] [1 12] [1 19] [12 5] [12 6] [12 7] [8 4]}])))))

(deftest series-topology-test
  (testing "Check series topology detection."
    (are [x] (series-member? x [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}])
         2 3 4 5 6 7)
    (are [x] (not (series-member? x [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}]))
         1 8))
  (testing "Check getting series from graph."
    (are [x y ser] (= (get-series x [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}])
                      (get-series y [#{[1 "ROOT"] [2 :a] [3 "A"] [4 :b] [5 "B"] [6 :c] [7 "C"] [8 "LOWER"]} #{[1 2] [1 4] [1 6] [2 3] [4 5] [6 7] [3 8] [5 8] [7 8]}])
                      ser)
         2 3 '(2 3)
         4 5 '(4 5)
         6 7 '(6 7)
         1 8 nil)))
