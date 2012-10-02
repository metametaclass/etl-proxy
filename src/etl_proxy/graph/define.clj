;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph.define
  (:use clojure.set))

;; ## Graph definition module.
;;
;; The main aim of this module is definition of graph data structure and basic functions above it.
;; Any graph in this program will defined as tuple of vertices set and edges set. Situation in which
;; some edge bind one or both vertices that doesn't members of vertices set will be considered as
;; error.
;;
;; Basic module data structure:
;;
;; - graph  := [#{vertex} #{edge}]
;; - vertex := [id body]
;; - edge   := [id id]
;; - route  := #{edge}
;; - id - unique natural number which must be reevaluated in every graph transformation.
;; - body - arbitrary data type which can express necessary abstract data.
;;

(def empty-graph [#{} #{}])

(defn vertices
  "This function return vertex set of graph."
  [graph]
  (first graph))

(defn edges
  "This function return edges set of graph."
  [graph]
  (second graph))

(defn ids
  "Create list of all graph indices."
  [graph]
  (set (map first (vertices graph))))

(defn bodies
  "Create vertices contents list with edges set lost."
  [graph]
  (set (map second (vertices graph))))

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

;; ## Graph transformation principle.
;;
;; Transformation at all is a intermediate process between two terminal forms of something. At this
;; point graph transformation is a process between to forms of graph. If we want to transform few
;; elements from graph instead of one, then we must save result in each step of transformation
;; process and merge all results in one at the end of process. The best way how we can do it is a
;; recursive procession.
;;
;; Let talk about recursion above graph at all. We can apply any function to graph which will return
;; new modified graph. We can process any such function with parameters list use each time
;; new graph as parameter for procession next list element by current function. If we will use
;; vertices set as a simple sequence of parameters, we get final graph as summarise result of
;; transformation (at current step). Main problem of such approach to process any elements patterns
;; that we can't say that procession of whole vertex list in current graph don't appear to new
;; vertices which doesn't satisfy pattern in resulted graph. So we need two patterns: one for
;; satisfying terminal graph state and one for decision of procession or not current vertex.
;;
;; So now as we can determine that all vertices are satisfy out pattern or not we can transform our
;; graph recursively until all nested bodies will deduced into their terminal state. To solve this
;; we will work with vertex set as with sequence of complex bodies.

(defn transform-graph
  "Recursively transform graph from one form into another.
  It accept following ever present parameters:

  * stop?
    ([graph])
    It is a function must return true when graph become to the desired form.

  * item-category
    ([graph])
    High order function which return sequence of items entered into specified category. Defaults
    values of this parameter is ids, bodies, vertices and edges functions.

  * item-filter? 
    ([item])
    This function must return true if you want to process accepted vertex.

  * item-processor
    ([item graph])
    This function will used for transform accepted item in the specified graph and return
    new modified graph as it result.

  Example: to rewrite your graph with capitalization of all strings use this:

    (defn all-vertices-are-capital? [graph] ...)
    (defn capitalize-vertex [body graph] ...)
    (transform-graph all-vertices-are-capital? bodies string? capitalize-vertex graph)"
  [stop? item-category item-filter? item-processor graph]
  (if (stop? graph)
    graph
    (recur stop?
           item-category
           item-filter?
           item-processor
           ((list-action-on-graph item-processor)
            (filter item-filter? (item-category graph))
            graph))))
