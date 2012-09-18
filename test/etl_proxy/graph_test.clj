(ns etl-proxy.graph_test
  (:use clojure.test
        etl-proxy.graph.crud
        etl-proxy.graph.route))

;; Define few graphs for our experiments.

(def tree-graph-example
  [#{[1 "Root"]
     [2 "Level 1 1"]
     [3 "Level 1 2"]
     [4 "Level 1 3"]
     [5 "Level 2 1"]
     [6 "Level 2 2"]
     [7 "Level 2 3"]
     [8 "Level 2 4"]
     [9 "Level 2 5"]}
   #{[1 2]
     [1 3]
     [1 4]
     [2 5]
     [2 6]
     [3 7]
     [4 8]
     [4 9]}])

(def route-graph-example
  [#{[1  "A"]
     [2  "B"]
     [3  "C"]
     [4  "D"]
     [5  "E"]
     [6  "F"]
     [7  "G"]
     [8  "H"]
     [9  "J"]
     [10 "K"]
     [11 "L"]
     [12 "M"]}
   #{[1  3]
     [1  4]
     [2  3]
     [2  4]
     [3  6]
     [3  5]
     [4  5]
     [4  8]
     [6  7]
     [5  7]
     [8  7]
     [8  9]
     [7  10]
     [10 9]
     [10 12]
     [11 10]}])

(deftest route-test

  (testing "Search route list from 1 to the 9 vertex in route-graph-example."
    (is (= (route-list 1 9 route-graph-example)
           (list #{[10 9] [3 5] [5 7] [1 3] [7 10]}
                 #{[10 9] [6 7] [1 3] [3 6] [7 10]}
                 #{[10 9] [4 5] [5 7] [7 10] [1 4]}
                 #{[8 7] [10 9] [7 10] [1 4] [4 8]}
                 #{[8 9] [1 4] [4 8]}))))

  (testing "Expand route list which already contain shortest route."
    (is (= (expand-route-tail 1
                              9
                              (list #{[3 5] [5 7] [1 3]}
                                    #{[6 7] [1 3] [3 6]}
                                    #{[4 5] [5 7] [1 4]}
                                    #{[8 7] [1 4] [4 8]}
                                    #{[8 9] [1 4] [4 8]})
                              route-graph-example)
           (list #{[3 5] [5 7] [1 3] [7 10]}
                 #{[6 7] [1 3] [3 6] [7 10]}
                 #{[4 5] [5 7] [7 10] [1 4]}
                 #{[8 7] [7 10] [1 4] [4 8]}
                 #{[8 9] [1 4] [4 8]})))))
