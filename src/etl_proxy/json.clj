;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.json
  (:use [etl-proxy.graph]
        [cheshire.core]))

;; ## Convert JSON markup language into graph data structure.
;;
;; This module was written for possibility of creation graph structure from json markup language
;; document. Internal mechanics of service require to represent any data structure as graph in set
;; terms. External communication has provided by JSON markup language and we need to transform it into
;; graph.
;;
;; With `cheshire` library clojure can represent json format as map composed of strings, vectors and
;; other maps. Because maps keys used as structure representation facility we can just to use values
;; in it. All dependencies in graph represented with edges set and keys safety doesn't necessary.
;;
;; Any vertex in graph can contain in its body terminal or non-terminal part of json document
;; structure. If vertex is non-terminal, then it mast be simplified.

;; ## Vertex simplification principle.
;;
;; I chose next rules for simplify vertices in graph:
;;
;; - non-sequences and lists will suppose as simplified vertices.
;; - any map will suppose as single nested vertex and it will transform to single list without maps
;;   keys (only values).
;; - any vector will suppose as vector of nested vertices and its each element will process in distinct
;;   single vertex manner.
;;

(defn simple-vertex?
  "Return true if accepted vertex doesn't contain nested vertices. For simplification rules visit project
  documentation."
  [body]
  (not (or (vector? body)
           (map? body))))

(defn simplify-vertex
  "Return single simplified parent vertex. For simplification rules visit project documentation."
  [body]
  (if (simple-vertex? body)
    body
    ;; Procession vector variant here is ignored because it will
    ;; solve in the rest of simplified vertex and doesn't occur here.
    (filter simple-vertex?
            (vals body))))

(defn rest-of-vertex
  "Return list of non simplified child vertices. For simplification rules visit project documentation."
  [body]
  ;; At this point we can accept only map because vectors
  ;; process element by element and doesn't return after
  ;; call this function.
  (if (simple-vertex? body)
    (list)
    (concat
     ;; All vector elements conjuction into single list. 
     (mapcat seq
             (filter #(vector? %)
                     (vals body)))
     ;; Maps will be added as is.
     (filter #(map? %)
             (vals body)))))

;; ## JSON processing principle.
;;
;; The simplest approach to recursive processing nested sequences is create graph with one vertex
;; without any edges in it. Then we need to simplify this vertex into one parent vertex and list of its
;; children vertices. Now we can create new graph without first vertex but with new vertices generated
;; above. Now we have graph contained same vertices and its edges which demonstrates vertices parent-child
;; relations. We reproduce our actions with current graph vertices until we have vertex set which we
;; can't simplify anymore.
;;
;; If we will process vertex set of graph in manner of sequence, than we can supply for each vertex its
;; simplified version and list of not simplified parts which was derived during simplification of
;; parent vertex. And all what we need now is substitution simplified vertex into not simplified and
;; then add not simplified childs to the new vertex.
;;
;; At this point we have problem we can't change body of any vertex without automatic changing its id.
;; This behaviour result in lost edges relation validity. I solve this problem in simple manner.
;; Instead of changing body of existed vertex we will add its simplified version as child AND as
;; parent to it, then add non simplified childs to simplified parent and finally TIE non simplified
;; vertex. Those action will result in automatic reevaluate edge indices in correct manner. When we tie
;; non-simplified vertex new edges appear excepting new vertex, because new simplified vertex fit as self
;; parent and child to non-simplified. There is two kinds of parasite edges. First is a parent-child
;; relation between A -> C objects when we already has A -> B -> C. So resulted graph mast be bring
;; to a light form of graph. Second is a self-loop relation A -> A object. Graph must be cleaned
;; from this relation type.

(defn simple-graph?
  "Return true if there is no non-terminal vertices in the graph."
  [graph]
  (= (list)
     ((fn [[vertices edges]]
        ;; Return list of non-terminal vertices from graph.
        (filter #(not (simple-vertex? %)) vertices))
      graph)))

(defn grow-sub-tree
  "Add simplified vertex and rest of it into graph and tie non-simplified vertex."
  [id graph]
  (let [body (body-by-id id graph)
        simple-body (simplify-vertex body)
        ;; Add simplified vertex as parent and child to the non-simplified vertex.
        new-graph (add-child id
                             simple-body
                             (add-parent id
                                         simple-body
                                         graph))
        simple-id (id-by-body simple-body new-graph)]
    (if-not (simple-vertex? body)
      (delete-obvious-edges
       (delete-self-loops
        (tie-vertex id                                  ;; Old non-simplified vertex.
                  (add-child-list simple-id           ;; New simplified vertex.
                                  (rest-of-vertex body) ;; It's non-simplified childs.
                                  new-graph))))
      graph)))

(defn simplify-graph
  "Accept graph data structure and simplify it recursively until graph become a simple variant of
  graph."
  [graph])

(defn graph-from-json
  "Accept json hash map and recursively convert it into graph."
  [json-map]
  (simplify-graph
   (add-vertex json-map empty-graph)))

;; USEME:
;; <pre><code>(parse-string (slurp "example.json") true)
;; </code></pre>
