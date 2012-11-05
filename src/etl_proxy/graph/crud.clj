;; Copyright (c) 2012  Malyshev Artem  <proofit404@gmail.com>

(ns etl-proxy.graph.crud
  (:use clojure.set
        etl-proxy.graph.define))

;; ## Create Read Update Delete graph module.
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

(defn available-id
  "Return single id which isn't among to any vertex."
  [graph]
  ((fn [a id-set]
     (if (contains? id-set a)
       (recur (inc a) id-set)
       a))
   1 ;; Default index if empty graph occurs.
   (ids graph)))

(defn graph-member?
  "If body belong to any vertex in the graph, then function return true."
  [body graph]
  (contains? (bodies graph) body))

(defn vertex-by-id
  "Find vertex by her id field. Return its as vector of id and body."
  [id graph]
  (first (select (fn [[vertex-id vertex-body]] (= id vertex-id)) (vertices graph))))

(defn body-by-id
  "Find vertex by its id field. Return its body as is."
  [id graph]
  ;; Get second element (vertex body) in first vertex in yield set.
  (second (vertex-by-id id graph)))

(defn id-by-body
  "Find vertex index by it content."
  [body graph]
  ;; Get first element (vertex id) in first vertex in yield set.
  (first (first (select (fn [[vertex-id vertex-body]] (= body vertex-body)) (vertices graph)))))

(defn relation-children
  "Return all vertices id with which current vertex is a parent (its children)."
  [id graph]
  (map second (filter (fn [[parent child]] (= id parent)) (edges graph))))

(defn relation-parents
  "Return all vertices id with which current vertex is a child (its parents)."
  [id graph]
  (map first (filter (fn [[parent child]] (= id child)) (edges graph))))

(defn edges-subset
  "Return set of edges children and parents correspond accepted two list."
  [parents-list children-list graph]
  (select (fn [[parent child]]
            (and (not= (list) (filter #(= parent %) parents-list))
                 (not= (list) (filter #(= child %) children-list))))
          (edges graph)))

;; ## Update section.
;;
;; Graph modification functions is self indexable function. You don't have to fix graph indices your
;; self. If you will try to add vertex which already exist in current graph or to add edges between
;; nonexistent vertices, then resultant graph doesn't change at all.

(defn add-vertex
  "Create new graph with specified vertex."
  [body graph]
  (if-not (graph-member? body graph)
    (vector (conj (vertices graph)
                  ;; This is new unique vertex for graph.
                  (vector (available-id graph) body))
            (edges graph))
    ;; OR return graph as is.
    graph))

(def add-vertices-list (list-action-on-graph add-vertex))

(defn add-edge
  "Create new graph with specified edge."
  [edge graph]
  (if ;; Check that both ids point to existent vertices.
      (subset? (set edge) (ids graph))
    (vector (vertices graph)
            (conj (edges graph) edge))
    ;; OR return graph as is.
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
  "Create new graph in which all passed vertices will be children of passed id."
  [id-parent item-list graph]
  (if (empty? item-list)
      graph
      (recur id-parent
             (rest item-list)
             (add-child id-parent
                        (first item-list)
                        graph))))

(defn add-child-series
  "Create new graph in which first passed vertex will be child of passed id, second passed vertex
  will be child of first, and each passed vertex will be a high-order parent for rest of the vertex
  list."
  [id-parent item-list graph]
  (if (empty? item-list)
      graph
      (let [cur-item (first item-list)
            new-graph (add-child id-parent cur-item graph)]
        (recur (id-by-body cur-item new-graph)
               (rest item-list)
               new-graph))))

;; ## Destroy section.

(defn delete-vertex
  "Delete vertex from graph, clear its edges."
  [id graph]
  (vector
   (disj (vertices graph) (vertex-by-id id graph))
   ;; Edges set without any object parent or child relations.
   (difference (edges graph)
               (select (fn [[parent child]] (or (= parent id) (= child id)))
                       (edges graph)))))

(def delete-vertices-list (list-action-on-graph delete-vertex))

(defn delete-edge
  "Delete edge (bind) between existing vertices."
  [edge graph]
  (vector (vertices graph) (disj (edges graph) edge)))

;; Main difference between tie and delete functions is in bind processing after vertex remove. For
;; example, we have three bound object A -> B -> C with parent -> child relation type and we need
;; remove B vertex from graph. Tie function will add A -> C relation into graph and delete function will
;; not. With delete function we will lost relation between A and C objects.

(defn tie-vertex
  "Delete vertex from graph, clear its edges and create edges between its parents and children for saving
  object relations without intermediate object."
  [id graph]
  ;; Create list of edges as direct multiply parent with child.
  (let [restore-edges (mapcat
                      (fn [parent]
                        (map #(vector parent %)
                             (relation-children id graph)))
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
        lower-level-children (mapcat #(relation-children % graph) roots)]
    (if (empty? lower-level-children)
      current-level-map
      (recur lower-level-children current-level-map))))

(defn delete-self-loops
  "Return clear graph without relations between objects with it self (self loops). For example for
  object A remove A -> A relation."
  [graph]
  (vector (vertices graph)
          (select (fn [[parent child]] (not= parent child))
                  (edges graph))))
