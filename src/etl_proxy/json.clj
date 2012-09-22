;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.json
  (:use cheshire.core
        etl-proxy.graph.crud
        etl-proxy.graph.route))

;; ## Convert JSON markup language into graph data structure.
;;
;; This module was written for possibility of creation graph structure from json markup language
;; document. Internal mechanics of service require to represent any data structure as graph in set
;; terms. External communication provide any data in the JSON markup language and we need to
;; transform it into graph.
;;
;; With `cheshire` library clojure can represent json format as map composed of strings, vectors and
;; other maps. We can convert this map into graph in recursive manner. From this point all vertices
;; can be divided into "simple" and "complex" by possibility of expression this vertex into graph
;; sub-tree.
;;
;; Any vertex in graph can contain in its body terminal or non-terminal part of json document
;; structure. If vertex is non-terminal, then it mast be simplified.

(defn simple-body?
  "Return true if accepted vertex can't be simplified. For simplification rules visit project
  documentation."
  [body]
  (not (or (list? body)
           (vector? body)
           (map? body))))

(defn simplify-body
  "Return list of simplified parent vertex fields. For simplification rules visit project
  documentation."
  [body]
  (if (map? body)
    ;; We return it as vector for observance vertex simplification principle.
    (apply vector
           (map (fn [[key value]]
                  (list key value))
                body))
    body))

;; ## Vertex simplification principle.
;;
;; I chose next rules for simplify vertices in graph:
;;
;; - non-sequence will suppose as simplified vertices.
;; - any map will suppose as few nested vertices and it will transform into sequences of lists for each
;;   key-value tuple vertices the same level graph relations. For example graph like A -> {:b B :c
;;   C :d D} -> E will result into A -> (:b B) -> E, A -> (:c C) -> E, A -> (:d D) -> E.
;; - any vector will suppose as few nested vertices and it will transform into sequences of its
;;   elements vertices the same level graph relations. For example if we have some graph like A ->
;;   [B C D] -> E than we will have resulted graph as A -> B -> E, A -> C -> E, A -> D -> E.
;; - list of non-sequences will suppose as parent-child relation series with "nearest element to
;;   start is high-order parent for rest of list" rule. For example if we have some graph like A ->
;;   (B C D) -> E then we will have resulted graph as A -> B -> C -> D -> E.
;;
;; I strongly recommend don't try get body of simplified element directly from graph in functions
;; below. It will be better to accept it as function parameter because with this approach becomes
;; possibility of it intermediate simplification with foreign function application.

(defn vector-simplification
  "This function simplify accepted id - body tuple in the vector manner."
  [id body graph]
  ;; At this function we must get vertex of vector type and add its elements as child for it.
  ;; Then we get list of old childs and list of newly added vector elements and add edges which
  ;; provide new element as new parents for all old child.
  (let [;; New graph with added newly elements as childs.
        new-graph (add-child-list id body graph)
        ;; List of old childs of accepted vector.
        restore-childs (relation-childs id graph)
        ;; List of new parents from vector elements.
        new-parents (map (fn [item] (id-by-body item new-graph)) body)
        ;; List of edges providing new parent relations (direct product of two lists above).
        new-relations (mapcat (fn [parent] (map (fn [child] (vector parent child)) restore-childs)) new-parents)]
    (add-edges-list new-relations new-graph)))

(defn map-simplification
  "This function simplify accepted id - body tuple in the hash-map manner."
  [id body graph]
  ;; Because hash-map can be adduced into vector by body simplifying function we just use its.
  (vector-simplification id (simplify-body body) graph))

(defn list-simplification
  "This function simplify accepted id - body tuple in the list manner."
  [id body graph]
  ;; At this function we must get vertex of list type and then process it in sequence manner. First
  ;; element of list must be a direct child of accepted vertex. Last element of list must be a
  ;; high-order parent to all childs of accepted vertex.
  (let [;; New graph with added newly element as childs series for accepted body.
        new-graph (add-child-series id body graph)
        ;; List of old childs of accepted vector.
        restore-childs (relation-childs id graph)
        ;; Last element in the series.
        new-parent (id-by-body (last body) new-graph)
        ;; List of edges providing last series element as new parent.
        new-relations (map (fn [child] (vector new-parent child)) restore-childs)]
    (add-edges-list new-relations new-graph)))

;; At this point we have problem we can't change body of any vertex without automatic changing its id.
;; This behaviour result in lost edges relation validity. I solve this problem in simple manner.
;; Instead of changing body of existed vertex we will add its simplified version as child _AND_ as
;; parent to it, then add non simplified childs to simplified parent and finally _TIE_ non simplified
;; vertex. Those action will result in automatic reevaluate edge indices in correct manner. When we tie
;; non-simplified vertex new edges appear excepting new vertex, because new simplified vertex fit as self
;; parent and child to non-simplified. There is two kinds of parasite edges. First is a parent-child
;; relation between A -> C objects when we already has A -> B -> C. So resulted graph mast be bring
;; to a light form of graph. Second is a self-loop relation A -> A object. Graph must be cleaned
;; from this relation type.

(defn simplify-vertex
  "Add simplified vertex body into graph and tie non-simplified vertex."
  [id graph]
  (let [body (body-by-id id graph)]
    (if (simple-body? body) 
      ;; Non-sequence will pass as is.
      graph
      ;; Sequence will process according to vertex simplification rules.
      (delete-obvious-edges
        (tie-vertex
         id ;; This is old non-simplified vertex id and it must be purged from graph.
         (cond
          ;; Any vector will "substitute" for sequence of its elements.
          (vector? body) (vector-simplification id body graph)
          ;; Any map will "substitute" for sequence of its simplified key-value tuple.
          (map? body) (map-simplification id body graph)
          ;; Any list will "substitute" for series of its elements.
          (list? body) (list-simplification id body graph)))))))

(def simplify-vertex-list (list-action-on-graph simplify-vertex))

;; ## JSON processing principle.
;;
;; So here we come to the recursive processing of given nested markup expressions. Let talk about
;; recursion above graph at all. We can apply any function to graph which will return new modified
;; (more simplified in our situation). We can process any such functions above lists of parameters
;; use each time new graph as parameter for procession next list element by current function. Main
;; problem of such approach to parse nested expressions that we can't say that procession all non
;; simplified list elements in current graph don't appear new non simplified vertices as resulted
;; graph. So we need check function for this trouble kind.

(defn simple-graph?
  "Return true if graph contain only simple vertices."
  [graph]
  (empty?
   ;; Return list of non-terminal vertices from graph.
   (filter #(not (simple-body? %)) (bodies graph))))

;; So now as we can determine that all vertices are simple or not we can simplify our graph
;; recursively until all nested expressions will deduced into their terminal state. To solve this we
;; will work with vertex set as with sequence of expressions.

(defn simplify-graph
  "Accept graph data structure and simplify it recursively until graph become a simple variant of
  graph."
  [graph]
  (if (simple-graph? graph)
    graph
    (recur
     (simplify-vertex-list
      ;; List of all non simplified vertices in the current graph.
      (map #(id-by-body % graph)
           (filter #(not (simple-body? %))
                   (bodies graph)))
      graph))))

(defn json2graph
  "Accept json string and recursively convert it into simple graph."
  [json]
  (simplify-graph
   (add-vertex
    (parse-string json)
    empty-graph)))
