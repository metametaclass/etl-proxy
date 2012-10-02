;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.json.generator
  (:use cheshire.core
        [etl-proxy.graph define comparison]))

;; ## Generate JSON mark up expression from graph data structure.
;;
;; As we see in json parser module main approach in graph conversion process is a implementation of
;; foreign markup expression into single vertex. Then we need to divide this "complex" vertex into
;; "simple" vertex set. Generation is opposite task for parsing of JSON markup document. So if we
;; have large graph where parent child relation carry out into edges set, then we need to compose
;; few binded vertices into one large "complex" vertex which body structure will contain parent
;; child relation expressed throw json markup part. So when we will have graph with unique vertex
;; and empty edges set we can approve that we has json expression.
;;
;; ## Vertices composition rules.
;;
;; So instead of vertex body type we need analyze vertex child relation structure. And at this point
;; we can to declare rules set below to express final json document throw finite number or its
;; application.
;;
;; - vertex with empty child list is a final json composition. If this vertex is a vector of
;;   '(key value) list, then it must be expressed into {:key value} map.
;; - all series must be composed into single list vertex. For example, if we have graph like A -> B
;;   -> C -> D, A -> E -> F -> D, A -> G -> H -> D then applying current rule to B, E and G vertex
;;   will result as A -> '(B C) -> D, A -> '(E F) -> D, A -> '(G H) -> D correspondingly.
;; - if few vertices has equal lists of parents and childs, then they must be conjunct into one
;;   vector. For example, if we has some graph like A -> B -> E, A -> C -> E, A -> D -> E, then
;;   current rule application will result into A -> [B C D] -> E graph.
;;

(defmulti compose-json
  "This function return new graph where accepted vertex will contain json map expressed from his
  parent-child relations."
  (fn [id graph]
    (cond
     (level-topology? id graph) :vector-rule
     (series-member? id graph) :curcuit-rule
     :else :terminal-rule)))

;; ## Down-Top processing approach.
;;
;; TODO: write documentation here. 

(defn json-expression?
  "This function return true when given graph contain unique vertex with json expression."
  [graph]
  (= 1 (count (vertices graph))))

(defn json-appearance?
  "This function return true when given vertex occur to graph part which can be composed into complex
  json expression."
  [id graph])

(defn graph2json
  "This function return json expression as string which correspond to given graph in topologically
  manner."
  [graph]
  (generate-string
   ;; Take unique vertex body as json expression.
   (first (bodies
           (transform-graph
            json-expression?
            ids
            json-appearance?
            compose-json
            graph)))))
