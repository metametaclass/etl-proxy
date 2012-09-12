;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph
  "# Graph processing module."
  (:use [clojure.set]))

;; The main aim of this module is definition of graph data structure and basic functions above
;; it. Any graph in this program will defined as tuple of nodes set and arcs set. Situation in which
;; some arc bind one or both nodes that doesn't members of nodes set will be considered as error.
;;
;; Basic module data structure:
;;
;; - graph := [#{node} #{arc}]
;; - node  := [id body]
;; - arc   := [id id]
;; - id - unique natural number which must be reevaluated in every graph transformation.
;; - body - arbitrary data type which can express necessary abstract data.
;; - route := #{arc}
;;

(def empty-graph [#{} #{}])

;; ## CRUD.
;; 
;; Design of this module suppose follow for the CRUD principle. Division module structure made with
;; correspond to "one volume equal one letter of CRUD word".

;; ## Create section.
;;
;; Better way to do this is write your own methods for converting exist data structures into our
;; graph representation. I think multimethods is a best way to implement such kind of parsers. For
;; example, if you have some tree-like data structure, then method accepted list of fields must
;; create single node with body which will useful for you. If method accept non-terminal element of
;; your tree, for example, list with nested lists, then you must create parent node that contain all
;; data and children's nodes that contain nested lists. When you call your multimethod recursively it
;; will parse tree-like data structure into graph until its leafs become terminal elements.
;;
;; Default way we do things above is accept single ISeq data structure and process it in below manner.
;;
;; If we accept list without nested lists, then we create graph with node from its elements. If we
;; accept list which contain nested sequences then all non-list elements is a body of root node and
;; all list elements is child's nodes.

;; ## Read section.

(defn ids
  "Create list of all graph indices."
  [[nodes arcs]]
  (set (map first nodes)))

(defn bodies
  "Create nodes contents list with arcs set lost."
  [[nodes arcs]]
  (set (map second nodes)))

(defn available-id
  "Return single id which isn't among to any node."
  [graph]
  (inc
   (reduce max
           ;; Add null index to the id set if empty graph is occur.
           (conj (ids graph) 0))))

(defn graph-member?
  "If body belong to any node in the graph, then function return true."
  [body graph]
  (contains? (bodies graph) body))

(defn node-by-id
  "Find node by her id field. Return its as vector of id and body."
  [id [nodes arcs]]
  (first (select (fn [[node-id node-body]] (= id node-id)) nodes)))

(defn body-by-id
  "Find node by her id field. Return its body as is."
  [id graph]
  ;; Get second element (node body) in first node in yield set.
  (second (node-by-id id graph)))

(defn id-by-body
  "Find node index by it content."
  [body [nodes arcs]]
  ;; Get first element (node id) in first node in yield set.
  (first (first (select (fn [[node-id node-body]] (= body node-body)) nodes))))

(defn relation-childs
  "Return all nodes id with which current node is a parent (its childs)."
  [id [nodes arcs]]
  (map second (filter (fn [[parent child]] (= id parent)) arcs)))

(defn relation-parents
  "Return all nodes id with which current node is a child (its parents)."
  [id [nodes arcs]]
  (map first (filter (fn [[parent child]] (= id child)) arcs)))

;; ## Update section.
;;
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

(defn add-parent
  "Add given node as parent to id in graph. If node already exist, then simple add arc (bind nodes)."
  [id-child body graph]
  (let [new-graph (add-node body graph)]
    (add-arc (vector (id-by-body body new-graph) id-child)
             new-graph)))

(defn add-child
  "Add given node as child to id in graph. If node already exist, then simple add arc (bind nodes)."
  [id-parent body graph]
  (let [new-graph (add-node body graph)]
    (add-arc (vector id-parent (id-by-body body new-graph))
             new-graph)))

;; ### Actions on list of items with graph.

(defn list-add-action
  "Create new graph comprising all new items from list processed by fun."
  [fun]
  (fn [item-list graph]
    (if (empty? item-list)
      graph
      (recur (rest item-list)
             (fun (first item-list) graph)))))

(def add-nodes-list (list-add-action add-node))

(def add-arcs-list (list-add-action add-arc))

(defn add-child-list
  "Create new graph in which all passed nodes will be childs of passed id."
  [id-parent item-list graph]
  (if (empty? item-list)
      graph
      (recur id-parent
             (rest item-list)
             (add-child id-parent
                        (first item-list)
                        graph))))

;; ## Destroy section.

(defn delete-arc
  "Delete arc (bind) between existing nodes."
  [arc [nodes arcs]]
  (vector nodes (disj arcs arc)))

;; Main difference between tie and delete functions is in bind processing after node remove. For
;; example, we have three bound object A -> B -> C with parent -> child relation type and we need
;; remove B node from graph. Tie function will add A -> C relation into graph and delete function will
;; not. With delete function we will lost relation between A and C objects.

(defn delete-node
  "Delete node from graph, clear its arcs."
  [id graph]
  ((fn [[nodes arcs]]
     (vector
      (disj nodes (node-by-id id graph))
      ;; Arcs set without any object parent or child relations.
      (difference arcs (select (fn [[parent child]] (or (= parent id) (= child id))) arcs))))
   graph))

(defn delete-nodes-list
  "Delete all nodes from delivered list and remove those arcs."
  [id-list graph]
  (if (empty? id-list)
    graph
    (recur (rest id-list)
           (delete-node (first id-list) graph))))

(defn tie-node
  "Delete node from graph, clear its arcs and create arcs between its parents and childs for saving
  object relations without intermediate object."
  [id graph]
  ;; Create list of arcs as direct multiply parent with child.
  (let [restore-arcs (mapcat
                      (fn [parent]
                        (map #(vector parent %)
                             (relation-childs id graph)))
                      (relation-parents id graph))]
    (delete-node id (add-arcs-list restore-arcs graph))))

;; Delete sub tree function can be used only with _LIST_ of roots because when it recursively call
;; itself such situation become common. Function for one root mast just to wrap this item into one
;; element list and recall itself. You can write such function if you need it as much.

(defn delete-sub-tree
  "Accept node id list every item in which point to the root of sub tree. Then delete all nodes and
  arcs belongs to those sub-tree."
  [roots graph]
  (let [;; Store graph without nodes accepted with roots.
        current-level-map (delete-nodes-list roots graph)
        ;; Store all child's from every roots node in the single list.
        lower-level-childs (mapcat #(relation-childs % graph) roots)]
    (if (empty? lower-level-childs)
      current-level-map
      (recur lower-level-childs current-level-map))))

(defn delete-self-loops
  "Return clear graph without relations between objects with it self (self loops). For example for
  object A remove A -> A relation."
  [[nodes arcs]]
  (vector nodes
          (select (fn [[parent child]] (not= parent child))
                  arcs)))

;; ## Graph route analyze tools.
;;
;; TODO: write documentation for route block.

(defn tie-route
  "Return route without arcs which contain accepted id as child or as parent. This function restore
  parent-child relations between remain objects without purged intermediate object. For example if
  we have relation below A -> B -> C and we tie B object then A -> C relation will be added."
  [id arcs]
  (let [restore-arcs
        (mapcat
         (fn [child]
           ;; Restore parent-child relation.
           (map #(vector % child)
                ;; Select all parents of tie id.
                (map first (select (fn [[parent child]] (= child id)) arcs))))
         ;; Select all childs of tie id.
         (map second (select (fn [[parent child]] (= parent id)) arcs)))]
    (difference
     (union arcs restore-arcs)
     ;; All arcs which contain id as parent or as child.
     (select (fn [[parent child]] (or (= parent id) (= child id))) arcs))))

(defn intermediate-id
  "Return any intermediate id from route except beginning and end points."
  [from to route]
  (first
   (select (fn [id]
             (and (not= from id)
                  (not= to id)))
           (union (set (map first route))
                  (set (map second route))))))

(defn route?
  "Return true if accepted arcs set is a route from A to B node."
  [from to arcs]
  (cond
   (= arcs #{[from to]}) true
   (= arcs #{}) false
   :else (recur from
                to
                (tie-route (intermediate-id from to arcs) arcs))))

(defn route-list
  "Return list of all possible routes from parent to child in graph."
  [from to graph]
  (let [routes (list)]
    ;; FIXME: I don't work yet.
    ))

(defn delete-obvious-arcs
  "Return clear graph without obvious direct relations between objects. For example for node sequence
  expressed through A -> B -> C and direct A -> C arcs last will deleted from graph."
  [graph]
  ((fn [[nodes arcs]]
     (vector nodes
             (select (fn [[parent child]]
                       (= (list [parent child])
                          (route-list parent child graph)))
                     arcs)))
   graph))
