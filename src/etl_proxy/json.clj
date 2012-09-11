;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.json
  "# Convert JSON markup language into graph data structure."
  (:use [etl-proxy.graph]
        [cheshire.core]))

;; This module was written for possibility of creation graph strucutre from json markup language
;; document. Internal mechanics of service require to represent any data structure as graph in set
;; terms. External communication has provided by JSON markup language and we need to transform it into
;; graph.
;;
;; With `cheshire` library clojure can represent json format as map composed of strings, vectors and
;; other maps. Because maps keys used as structure representation facility we can just to use values
;; in it. All dependencies in graph represented with arcs set and keys safity doesn't necessary.
;;
;; Any node in graph can contain in its body terminal or non-terminal part of json document
;; structure. If node is non-terminal, then it mast be simplified.

;; ## Node simplification principle.
;;
;; I chose next rules for simplify nodes in graph:
;;
;; - non-sequences and lists will suppose as simplified nodes.
;; - any map will suppose as single nested node and it will transform to single list without maps
;;   keys (only values).
;; - any vector will suppose as vector of nested nodes and its each element will process in distinct
;;   single node manner.
;;

(defn simple-node?
  "Return true if accepted node doesn't contain nested nodes. For simplification rules visit project
  documentation."
  [body]
  (not (or (vector? body)
           (map? body))))

(defn simplify-node
  "Return single simplified parent node. For simplification rules visit project documentation."
  [body]
  (if (simple-node? body)
    body
    ;; Procession vector variant here is ignored because it will
    ;; solve in the rest of simplified node and doesn't occur here.
    (filter simple-node?
            (vals body))))

(defn rest-of-node
  "Return list of non simplified child nodes. For simplification rules visit project documentation."
  [body]
  ;; At this point we can accept only map because vectors
  ;; process element by element and doesn't return after
  ;; call this fucntion.
  (if (simple-node? body)
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
;; The simplest approach to recursive processing nested sequinces is create graph with one node
;; without any arcs in it. Then we need to simplify this node into one parent node and list of its
;; children nodes. Now we can create new graph without first node but with new nodes generated
;; above. Now we have graph contained same nodes and its arcs which demonstrates nodes parent-child
;; relations. We reproduse our actions with current graph nodes until we have node set which we
;; can't simplify anymore.
;;
;; If we will process node set of graph in manner of sequence, than we can supply for each node its
;; simplified version and list of not simplified parts which was derived during simplification of
;; parent node. And all what we need now is substitution simplified node into not simplified and
;; then add not simplified childs to the new node.
;;
;; At this point we have problem we can't change body of any node without automatic changing its id.
;; This behaviour result in lost arcs relation validity. I solve this problem in simple manner.
;; Instead of changing body of existed node we will add its simplified version as child AND as
;; parent to it, then add non simplified childs to simplified parent and finaly TIE non simplified
;; node. Those action will result in automatic reevaluate arc indices in correct manner. When we tie
;; non-simplified node new arcs appear excepting new node, because new simplified node fit as self
;; parent and child to non-simplified. There is two kinds of parasite arcs. First is a parent-child
;; relation between A -> C objects when we already has A -> B -> C. So resulted graph mast be bring
;; to a light form of graph. Second is a self-loop relation A -> A object. Graph must be cleaned
;; from this relation type.

(defn simple-graph?
  "Return true if there is no non-terminal nodes in the graph."
  [graph]
  (= (list)
     ((fn [[nodes arcs]]
        ;; Return list of non-terminal nodes from graph.
        (filter #(not (simple-node? %)) nodes))
      graph)))

(defn grow-sub-tree
  "Add simplified node and rest of it into graph and tie non-simplified node."
  [id graph]
  (let [body (body-by-id id graph)
        simple-body (simplify-node body)
        ;; Add simplified node as parent and child to the non-simplified node.
        new-graph (add-child id
                             simple-body
                             (add-parent id
                                         simple-body
                                         graph))
        simple-id (id-by-body simple-body new-graph)]
    (if-not (simple-node? body)
      (tie-node id ;; Old non-simplified node.
                (add-child-list simple-id ;; New simplified node.
                                (rest-of-node body) ;; Its non-simplified childs.
                                new-graph))
      graph)))

(defn simplify-graph
  "Accept graph data structure and simplify it recursively until graph become a simple variant of
  graph."
  [graph])

(defn graph-from-json
  "Accept json hash map and recursively convert it into graph."
  [json-map]
  (simplify-graph
   (add-node json-map empty-graph)))

;; USEME:
;; <pre><code>(parse-string (slurp "example.json") true)
;; </code></pre>
