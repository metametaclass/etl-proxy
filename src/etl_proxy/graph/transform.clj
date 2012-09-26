;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph.transform
  (:use etl-proxy.graph.crud))

;; ## Graph transformation module.
;;
;; Transformation at all is a intermediate process between two terminal forms of something. At this
;; point graph transformation is a process between to forms of graph. If be want to transform few
;; elements from graph instead of one, then we must save result in each step of transformation
;; process and merge all results in one at the end of process. The best way how we can do it is a
;; recursive procession.
;;
;; Let talk about recursion above graph at all. We can apply any function to graph which will return
;; new modified graph. We can process any such function with parameters list use each time
;; new graph as parameter for procession next list element by current function. If we will use
;; vertices set as a simple sequence of parameters, we get final graph as summarise result of
;; transformation (at current step). Main problem of such approach to process any vertex patterns
;; that we can't say that procession of whole elements list in current graph don't appear to new
;; vertices which doesn't satisfy pattern in resulted graph. So we need two patterns one for
;; satisfying terminal graph state and one for process or not current separate vertex decision.

(defmulti transform-vertex
  "This function process graph id element in manner specified by passed body type. It return new
  graph as result of it evaluation."
  (fn [id graph]
    (class (body-by-id id graph))))

(def transform-vertex-list
  "Process graph with vertex list in recursive manner."
  (list-action-on-graph transform-vertex))

;; So now as we can determine that all vertices are satisfy out pattern or not we can transform our
;; graph recursively until all nested bodies will deduced into their terminal state. To solve this
;; we will work with vertex set as with sequence of complex bodies.

(defn transform-graph
  "Recursively transform graph into other graph form based on multimethods approach.
   It accept following ever present parameters:

   * stop?
     [graph]
     It is a function at true value of which result recursive call will stop.

   * cur-item? 
     [body]
     It is a function which will used for filter vertices for processing later."
  [stop? cur-item? graph]
  (if (stop? graph)
    graph
    (recur stop? cur-item? (transform-vertex-list
                            (map (fn [body] (id-by-body body graph))
                                 (filter cur-item? (bodies graph)))
                            graph))))
