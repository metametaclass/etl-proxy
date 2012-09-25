;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

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
