;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph.transform
  (:use etl-proxy.graph.crud))

;; ## Graph transformation module.
;;
;; Let talk about recursion above graph at all. We can apply any function to graph which will return
;; new modified graph. We can process any such functions above lists of parameters use each time
;; new graph as parameter for procession next list element by current function. Main problem of
;; such approach to process nested body's patterns that we can't say that procession all list
;; elements in current graph don't appear to new vertices in resulted graph in which some vertex
;; will don't satisfy our processing pattern. So we need check function for this trouble kind.

;; TODO: Write documentation for vertex per vertex procession.

(defmulti vertex-processor
  "This function process graph id element in manner specified by passed body type. It return new
  graph as result of it evaluation."
  (fn [id graph]
    (class (body-by-id id graph))))

(def process-vertex-list
  "Process graph with vertex list in recursive manner."
  (list-action-on-graph vertex-processor))

;; So now as we can determine that all vertices are satisfy out pattern or not we can transform our
;; graph recursively until all nested bodies will deduced into their terminal state. To solve this
;; we will work with vertex set as with sequence of complex bodies.

(defn transform-graph
  "Recursively transform graph into other graph form based on multimethods approach.
   It accept following ever present parameters:
   * stop?
     -----
     [graph]
     It is a function at true value of which result recursive call will stop.
   * cur-item? 
     ---------
     [body]
     It is a function which will used for filter vertices for processing later."
  [stop? cur-item? graph]
  (if (stop? graph)
    graph
    (recur stop? cur-item? (process-vertex-list
                            (map (fn [body] (id-by-body body graph))
                                 (filter cur-item? graph))
                            graph))))
