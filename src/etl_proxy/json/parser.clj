;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.json.parser
  (:use cheshire.core
        [etl-proxy.graph define crud route]))

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
;; sub-tree. If vertex has "complex" body, then it mast be simplified.
;;
;; ## Vertex simplification principle.
;;
;; I chose next rules for simplify vertices in graph:
;;
;; - non-sequence will suppose as simplified vertices.
;; - any vector will suppose as few nested vertices and it will transform into sequences of its
;;   elements vertices the same level graph relations. For example if we have some graph like A ->
;;   [B C D] -> E than we will have resulted graph as A -> B -> E, A -> C -> E, A -> D -> E.
;; - any map will transform into vector of lists for each key-value tuple vertices the same level
;;   graph relations. For example graph like A -> {:b B :c C :d D} -> E will result into A -> (:b B)
;;   -> E, A -> (:c C) -> E, A -> (:d D) -> E. This graph will be a result of next rules applying to
;;   it as to input data.
;; - list of non-sequences will suppose as parent-child relation series with "nearest element to
;;   start is high-order parent for rest of list" rule. For example if we have some graph like A ->
;;   (B C D) -> E then we will have resulted graph as A -> B -> C -> D -> E.
;;
;; At this point we have problem we can't change body of any vertex without automatic changing its id.
;; This behaviour result in lost edges relation validity. I solve this problem in simple manner.
;; Instead of changing body of existed vertex we will add its simplified version as child _AND_ as
;; parent to it, then add non simplified childs to simplified parent and finally _TIE_ non simplified
;; vertex. Those action will result in automatic reevaluate edge indices in correct manner. When we tie
;; non-simplified vertex new edges appear excepting new vertex, because new simplified vertex fit as self
;; parent and child to non-simplified. There is some kinds of parasite edges appearing throw this
;; action. It is a parent-child relation between A -> C objects when we already has A -> B -> C. So
;; resulted graph mast be bring to a light form of graph.

(defprotocol JsonParser
  (decompose-json [body graph]
    "This function process graph element in manner specified by passed body type. It return new
  graph as result of it evaluation."))

(extend-type clojure.lang.IPersistentVector
  JsonParser
  (decompose-json [body graph]
  ;; At this function we must get vertex of vector type and add its elements as child for it.
  ;; Then we get list of old childs and list of newly added vector elements and add edges which
  ;; provide new element as new parents for all old child.
  (let [;; Old body which must be purged from graph.
        id (id-by-body body graph)
        ;; New graph with added newly elements as childs.
        new-graph (add-child-list id body graph)
        ;; List of old childs of accepted vector.
        restore-childs (relation-childs id graph)
        ;; List of new parents from vector elements.
        new-parents (map (fn [item] (id-by-body item new-graph)) body)
        ;; List of edges providing new parent relations (direct product of two lists above).
        new-relations (mapcat (fn [parent] (map (fn [child] (vector parent child)) restore-childs)) new-parents)]
    (delete-obvious-edges
     (tie-vertex
      id ;; This is old non-simplified vertex id and it must be purged from graph.
      (add-edges-list new-relations new-graph))))))

(extend-type clojure.lang.IPersistentMap
  JsonParser
  (decompose-json [body graph]
  ;; This function simplify accepted id's body type in the hash-map manner. As we accept it we must
  ;; simple substitute it into same tuples vector of key-value lists extracted from current map.
  (let [;; Old body which must be purged from graph.
        id (id-by-body body graph)
        ;; Because hash-map can be adduced into vector by body simplifying function we just use its.
        new-body (apply vector (map (fn [[key value]] (list key value)) (body-by-id id graph)))
        ;; New graph with body adduced into vector form.
        new-graph (add-child id new-body graph)
        ;; List of childs of accepted map.
        restore-childs (relation-childs id graph)
        ;; List of edges which designate parent relation with new body.
        new-relations (map (fn [child]
                             (vector (id-by-body new-body new-graph) child))
                           restore-childs)]
    (delete-obvious-edges
     (tie-vertex
      id ;; This is old non-simplified vertex id and it must be purged from graph.
      (add-edges-list new-relations new-graph))))))

(extend-type clojure.lang.IPersistentList
  JsonParser
  (decompose-json [body graph]
  ;; At this function we must get vertex of list type and then process it in sequence manner. First
  ;; element of list must be a direct child of accepted vertex. Last element of list must be a
  ;; high-order parent to all childs of accepted vertex.
  (let [;; Old body which must be purged from graph.
        id (id-by-body body graph)
        ;; New graph with added newly element as childs series for accepted body.
        new-graph (add-child-series id body graph)
        ;; List of old childs of accepted vector.
        restore-childs (relation-childs id graph)
        ;; Last element in the series.
        new-parent (id-by-body (last body) new-graph)
        ;; List of edges providing last series element as new parent.
        new-relations (map (fn [child] (vector new-parent child)) restore-childs)]
    (delete-obvious-edges
     (tie-vertex
      id ;; This is old non-simplified vertex id and it must be purged from graph.
      (add-edges-list new-relations new-graph))))))

;; ## JSON processing principle.
;;
;; So here we come to the recursive processing of given nested markup expressions. To call function
;; in recursive manner or not we must have any data to pass it as parameter at first time. Cheshire
;; library parse string into single map. We can suppose it as graph with single vertex and without
;; any edges. Then we define some rules following which we can split it into few smaller subgraphs
;; and carry out their relations from hash-map hierarchy to the edges set. If we use graph
;; transformation principle then we must have to predicate functions. First will return true when
;; all vertices in the graph are "simple". It's a recursion stop predicate. Second will return true
;; if accepted vertex body must be simplified into new subgraph.

(defn json-container?
  "Return true if accepted vertex can't be simplified. For simplification rules visit project
  documentation."
  [body]
  (or (list? body)
      (vector? body)
      (map? body)))

(defn json-free?
  "Return true if graph contain only simple vertices."
  [graph]
  (empty?
   ;; Return list of non-terminal vertices from graph.
   (filter (fn [body] (json-container? body))
           (bodies graph))))

(defn json2graph
  "Accept json string and recursively convert it into simple graph."
  [json]
  (transform-graph
   json-free?
   json-container?
   decompose-json
   (add-vertex (parse-string json) empty-graph)))
