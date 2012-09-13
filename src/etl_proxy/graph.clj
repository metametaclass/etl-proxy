;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph
  (:use [clojure.set]))

;; ## Graph processing module.
;;
;; The main aim of this module is definition of graph data structure and basic functions above
;; it. Any graph in this program will defined as tuple of vertices set and edges set. Situation in which
;; some edge bind one or both vertices that doesn't members of vertices set will be considered as error.
;;
;; Basic module data structure:
;;
;; - graph := [#{vertex} #{edge}]
;; - vertex  := [id body]
;; - edge   := [id id]
;; - route := #{edge}
;; - id - unique natural number which must be reevaluated in every graph transformation.
;; - body - arbitrary data type which can express necessary abstract data.
;;

(def empty-graph [#{} #{}])

;; All graph modification must accomplish with functions which accept one processed item and
;; intended graph. All of this functions return new modified graph as result. If you have some sort
;; of such function and you need to process list of items, then you can use genetic definition
;; below. It is a simple way to recursive process graph.

(defn list-action-on-graph
  "Create new graph comprising all new items from list processed by fun."
  [fun]
  (fn [item-list graph]
    (if (empty? item-list)
      graph
      (recur (rest item-list)
             (fun (first item-list) graph)))))

;; ## CRUD.
;; 
;; Design of this module suppose follow for the CRUD principle. Division module structure made with
;; correspond to "one volume equal one letter of CRUD word".

;; ## Create section.
;;
;; Better way to do this is write your own methods for converting exist data structures into our
;; graph representation. I think multimethods is a best way to implement such kind of parsers. For
;; example, if you have some tree-like data structure, then method accepted list of fields must
;; create single vertex with body which will useful for you. If method accept non-terminal element of
;; your tree, for example, list with nested lists, then you must create parent vertex that contain all
;; data and children's vertices that contain nested lists. When you call your multimethod recursively it
;; will parse tree-like data structure into graph until its leafs become terminal elements.
;;
;; Default way we do things above is accept single ISeq data structure and process it in below manner.
;;
;; If we accept list without nested lists, then we create graph with vertex from its elements. If we
;; accept list which contain nested sequences then all non-list elements is a body of root vertex and
;; all list elements is child's vertices.

;; ## Read section.

(defn ids
  "Create list of all graph indices."
  [[vertices edges]]
  (set (map first vertices)))

(defn bodies
  "Create vertices contents list with edges set lost."
  [[vertices edges]]
  (set (map second vertices)))

(defn available-id
  "Return single id which isn't among to any vertex."
  [graph]
  (inc
   (reduce max
           ;; Add null index to the id set if empty graph is occur.
           (conj (ids graph) 0))))

(defn graph-member?
  "If body belong to any vertex in the graph, then function return true."
  [body graph]
  (contains? (bodies graph) body))

(defn vertex-by-id
  "Find vertex by her id field. Return its as vector of id and body."
  [id [vertices edges]]
  (first (select (fn [[vertex-id vertex-body]] (= id vertex-id)) vertices)))

(defn body-by-id
  "Find vertex by her id field. Return its body as is."
  [id graph]
  ;; Get second element (vertex body) in first vertex in yield set.
  (second (vertex-by-id id graph)))

(defn id-by-body
  "Find vertex index by it content."
  [body [vertices edges]]
  ;; Get first element (vertex id) in first vertex in yield set.
  (first (first (select (fn [[vertex-id vertex-body]] (= body vertex-body)) vertices))))

(defn relation-childs
  "Return all vertices id with which current vertex is a parent (its childs)."
  [id [vertices edges]]
  (map second (filter (fn [[parent child]] (= id parent)) edges)))

(defn relation-parents
  "Return all vertices id with which current vertex is a child (its parents)."
  [id [vertices edges]]
  (map first (filter (fn [[parent child]] (= id child)) edges)))

(defn edges-subset
  "Return set of edges childs and parents correspond accepted two list."
  [parents-list childs-list [vertices edges]]
  (select (fn [[parent child]]
            (and (not= (list) (filter #(= parent %) parents-list))
                 (not= (list) (filter #(= child %) childs-list))))
          edges))

;; ## Update section.
;;
;; Graph modification functions is self indexable function. You don't have to fix graph indices your
;; self. If you will try to add vertex which already exist in current graph or to add edges between
;; nonexistent vertices, then resultant graph doesn't change at all.

(defn add-vertex
  "Create new graph with specified vertex."
  [body graph]
  (if-not (graph-member? body graph)
    ((fn [[vertices edges]]
       (vector (conj vertices
                     ;; This is new unique vertex for graph.
                     (vector (available-id graph) body))
               edges))
     ;; Apply conjunction of new vertex to graph.
     graph)
    ;; Return graph as is.
    graph))

(def add-vertices-list (list-action-on-graph add-vertex))

(defn add-edge
  "Create new graph with specified edge."
  [edge graph]
  (if ;; Check that both ids point to existent vertices.
      (subset? (set edge) (ids graph))
    ((fn [[vertices edges]]
       (vector vertices
               (conj edges edge)))
     ;; Apply conjunction of new edge to graph.
     graph)
    ;; Return graph as is.
    graph))

(def add-edges-list (list-action-on-graph add-edge))

(defn add-parent
  "Add given vertex as parent to id in graph. If vertex already exist, then simple add edge (bind vertices)."
  [id-child body graph]
  (let [new-graph (add-vertex body graph)]
    (add-edge (vector (id-by-body body new-graph) id-child)
             new-graph)))

(defn add-child
  "Add given vertex as child to id in graph. If vertex already exist, then simple add edge (bind vertices)."
  [id-parent body graph]
  (let [new-graph (add-vertex body graph)]
    (add-edge (vector id-parent (id-by-body body new-graph))
             new-graph)))

(defn add-child-list
  "Create new graph in which all passed vertices will be childs of passed id."
  [id-parent item-list graph]
  (if (empty? item-list)
      graph
      (recur id-parent
             (rest item-list)
             (add-child id-parent
                        (first item-list)
                        graph))))

;; ## Destroy section.

(defn delete-vertex
  "Delete vertex from graph, clear its edges."
  [id graph]
  ((fn [[vertices edges]]
     (vector
      (disj vertices (vertex-by-id id graph))
      ;; Edges set without any object parent or child relations.
      (difference edges (select (fn [[parent child]] (or (= parent id) (= child id))) edges))))
   graph))

(def delete-vertices-list (list-action-on-graph delete-vertex))

(defn delete-edge
  "Delete edge (bind) between existing vertices."
  [edge [vertices edges]]
  (vector vertices (disj edges edge)))

;; Main difference between tie and delete functions is in bind processing after vertex remove. For
;; example, we have three bound object A -> B -> C with parent -> child relation type and we need
;; remove B vertex from graph. Tie function will add A -> C relation into graph and delete function will
;; not. With delete function we will lost relation between A and C objects.

(defn tie-vertex
  "Delete vertex from graph, clear its edges and create edges between its parents and childs for saving
  object relations without intermediate object."
  [id graph]
  ;; Create list of edges as direct multiply parent with child.
  (let [restore-edges (mapcat
                      (fn [parent]
                        (map #(vector parent %)
                             (relation-childs id graph)))
                      (relation-parents id graph))]
    (delete-vertex id (add-edges-list restore-edges graph))))

(def tie-vertices-list (list-action-on-graph tie-vertex))

;; Delete sub tree function can be used only with _LIST_ of roots because when it recursively call
;; itself such situation become common. Function for one root mast just to wrap this item into one
;; element list and recall itself. You can write such function if you need it as much.

(defn delete-sub-tree
  "Accept vertex id list every item in which point to the root of sub tree. Then delete all vertices and
  edges belongs to those sub-tree."
  [roots graph]
  (let [;; Store graph without vertices accepted with roots.
        current-level-map (delete-vertices-list roots graph)
        ;; Store all child's from every roots vertex in the single list.
        lower-level-childs (mapcat #(relation-childs % graph) roots)]
    (if (empty? lower-level-childs)
      current-level-map
      (recur lower-level-childs current-level-map))))

(defn delete-self-loops
  "Return clear graph without relations between objects with it self (self loops). For example for
  object A remove A -> A relation."
  [[vertices edges]]
  (vector vertices
          (select (fn [[parent child]] (not= parent child))
                  edges)))

;; ## Graph route analyze tools.
;;
;; TODO: write documentation for route block.

(defn tie-route
  "Return route without edges which contain accepted id as child or as parent. This function restore
  parent-child relations between remain objects without purged intermediate object. For example if
  we have relation below A -> B -> C and we tie B object then A -> C relation will be added."
  [id edges]
  (let [restore-edges
        (mapcat
         (fn [child]
           ;; Restore parent-child relation.
           (map #(vector % child)
                ;; Select all parents of tie id.
                (map first (select (fn [[parent child]] (= child id)) edges))))
         ;; Select all childs of tie id.
         (map second (select (fn [[parent child]] (= parent id)) edges)))]
    (difference
     (union edges restore-edges)
     ;; All edges which contain id as parent or as child.
     (select (fn [[parent child]] (or (= parent id) (= child id))) edges))))

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
  "Return true if accepted edges set is a route from A to B vertex."
  [from to edges]
  (cond
   (= edges #{[from to]}) true
   (= edges #{}) false
   :else (recur from
                to
                (tie-route (intermediate-id from to edges) edges))))

(defn route-list
  "Return list of all possible routes from parent to child in graph."
  [from to graph])

(defn delete-obvious-edges
  "Return clear graph without obvious direct relations between objects. For example for vertex sequence
  expressed through A -> B -> C and direct A -> C edges last will deleted from graph."
  [graph]
  ((fn [[vertices edges]]
     (vector vertices
             (select (fn [[parent child]]
                       (= (list [parent child])
                          (route-list parent child graph)))
                     edges)))
   graph))
