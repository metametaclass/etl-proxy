;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graphs
  "Graph processing module."
  (:use [clojure.set]))

;;; Graph processing library.

;; The main aim of this module is definition of graph data structure and basic functions above it. Any
;; graph in this program will defined as tuple of nodes set and arcs set written in terms of
;; list. Situation in which some arc bind one or both nodes that doesn't members of nodes set will be
;; considered as error.

;; Design of this module suppose follow for the CRUD principle. Division module structure made with
;; correspond to "one volume equal one letter of CRUD word".

;; Basic data structure:
;; * graph := [(node) (arc)]
;; * node  := [id body]
;; * arc   := [id id]
;; * id - unique natural number which must be reevaluated in every time of graph transformation. In
;;   future modules.
;; * body - arbitrary data type which can express necessary abstract data.

(def empty-graph [#{} #{}])


;;; Create section.

(defn graph
  "Create graph from list of nodes. Repetitive items with same body will represent as single node."
  [& bodies]
  (vector
   ;; Create nodes set with tuples of positive natural number and node body.
   ;; Repetitive nodes will remove during set operations.
   (set (map vector (range (count bodies)) (set bodies)))
   ;; FIXME: Empty set of arcs.
   (set (list))))

(defn arc
  "Create arc from tuple of nodes."
  [parent child]
  (vector (first parent) (first child)))

(defn merge-graphs
  "Take few graph and create new graph which contain all distinct nodes and arcs."
  [& graphs]
  ;; TODO: arcs sets merge, they lost now...
  (apply graph (mapcat bodies graphs)))


;;; Read section.

(defn graph-member?
  "If body belong to any node in the graph, then function return true."
  [body graph]
  (contains? (bodies graph) body))

(defn available-id
  "Return single id which isn't among to any node."
  [graph]
  (inc (reduce max (ids graph))))

(defn ids
  "Create list of all graph indices."
  [[nodes arcs]]
  (set (map first nodes)))

(defn bodies
  "Create nodes contents list with arcs set lost."
  [[nodes arcs]]
  (set (map second nodes)))

(defn body-by-id
  "Find node by her id field. Return its as vector of id and body."
  [id [nodes arcs]]
  ;; Get second elemend (node body) in first node in yield set.
  (second (first (select (fn [[node-id node-body]] (= id node-id)) nodes))))

(defn id-by-body
  "Find node index by it content."
  [body [nodes arcs]]
  ;; Get first elemend (node id) in first node in yield set.
  (first (first (select (fn [[node-id node-body]] (= body node-body)) nodes))))

(defn relation-childs
  "Return all nodes id with which current node is a parent (its childs)."
  [id [nodes arcs]]
  (map second (filter (fn [[parent child]] (= id parent)) arcs)))

(defn relation-parents
  "Return all nodes id with which current node is a child (its parents)."
  [id [nodes arcs]]
  (map first (filter (fn [[parent child]] (= id child)) arcs)))


;;; Update section.

;; Graph modification functions is self indexable function. You don't have to fix graph indices your
;; self. If you will try to add node which already exist in current graph or to add arcs between
;; nonexistent nodes, then resultant graph doesn't change at all.

(defn add-node
  "Create new graph with specified node."
  [body graph]
  (if-not (graph-member? body graph)
    ((fn [[nodes arcs]]
       (vector (conj nodes
                     ;; This is new unique node for graph.
                     (vector (available-id graph) body))
               arcs))
     ;; Apply conjunction of new node to graph.
     graph)
    ;; Return graph as is.
    graph))

(defn add-arc
  "Create new graph with specified arc."
  [arc graph]
  (if ;; Check that both ids point to existent nodes.
      (subset? (set arc) (ids graph))
    ((fn [[nodes arcs]]
       (vector nodes
               (conj arcs arc)))
     ;; Apply conjunction of new arc to graph.
     graph)
    ;; Return graph as is.
    graph))


;;; Destroy section.

(defn delete-arc
  "Delete arc (bind) between existing nodes."
  [arc [nodes arcs]]
  [nodes (filter #(not= arc %) arcs)])

;; Main difference between tie and delete functions is in bind processing after node remove. For
;; example, we have three bound object A -> B -> C with parent -> child relation type and we need
;; remove B node from graph. Tie function will add A -> C relation into graph and delete function will
;; not. With delete function we will lost relation between A and C objects.

(defn tie-node
  "Delete node from graph, clear its arcs and create arcs between its parents and childs for saving
object relations without intermediate object."
  [id [nodes arcs]]
  (let
      [tree (vector nodes arcs)
       ;; Create list of arcs as direct multiply parent with child.
       restore-arcs (mapcat
                     (fn [parent]
                       (map #(vector parent %)
                            (relation-childs id tree)))
                     (relation-parents id tree))]
    [(filter #(not= id (first %)) nodes)
     (filter (fn [[parent child]]
               (and (not= parent id)
                    (not= child id)))
             (concat arcs restore-arcs))]))

(defn delete-node
  "Delete node from graph, clear its arcs."
  [id [nodes arcs]]
  [(filter #(not= id (first %)) nodes)
   (filter (fn [[parent child]]
             (and (not= parent id)
                  (not= child id))) arcs)])

(defn delete-nodes-list
  "Delete all nodes from delivered list and remove those arcs."
  [id-list tree]
  (if (empty? id-list)
    tree
    (recur (rest id-list)
           (delete-node (first id-list) tree))))

;; Delete sub tree function can be used only with _LIST_ of roots because when it recursively call
;; itself such situation become common. Function for one root mast just to wrap this item into one
;; element list and recall itself. You can write such function if you need it as much.

(defn delete-sub-tree
  "Accept node id list every item in which point to the root of sub tree. Then delete all nodes and
arcs belongs to those sub-tree."
  [id-list tree]
  (let [;; Store graph without nodes accepted with id-list.
        current-level-map (delete-nodes-list id-list tree)
        ;; Store all childs from every id-list node in the single list.
        lower-level-childs (mapcat #(relation-childs % tree) id-list)]
    (if (empty? lower-level-childs)
      current-level-map
      (recur lower-level-childs current-level-map))))
