;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph.route
  (:use [etl-proxy.graph define crud]
        clojure.set))

;; ## Graph route analyze tools.
;;
;; In our graph processing module we represent route from one vertex to other through set of edges
;; between several intermediate vertices. We doesn't have any wight variable at any edge so shorter
;; route is a route with less number of edges in it. Normally each intermediate node must appear
;; twice as the child of one edge and as parent of following it.

;; Tie operation is simplest operation on the routes. Its result as new shorter route with out one
;; id in it. So if we have route a1 -> a2 -> a3 -> ... -> an then we will get shorter route after
;; tie a2 vertex. It will result as a1 -> a3 -> ... -> an. Throw this simple operation we can
;; control all other properties of route. We conclude that given set of edges is a route if we know
;; start and end vertex of route and we can tie all intermediate vertices (not start or end
;; vertices)  into single edge start -> end.

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
  [start end route]
  (first
   (select (fn [id]
             (and (not= start id)
                  (not= end id)))
           (union (set (map first route))
                  (set (map second route))))))

(defn route?
  "Return true if accepted edges set is a route from A to B vertex."
  [start end edges]
  (cond
   (= edges #{[start end]}) true
   (or
    (= edges #{})
    (= (count edges) 1)) false
    :else (recur start
                 end
                 (tie-route (intermediate-id start end edges) edges))))

;; If we sure that given edge set is a route and we get it start vertex, then we can evaluate end
;; vertex in the route by tie child of start vertex until we got route consist from single edge.

(defn route-end
  "Return final vertex in the route."
  [start route]
  (cond
   ;; Get child relation from unique element of route set.
   (= (count route) 1) (second (first route))
   ;; Tie child relation of start and try one more time.
   :else (recur start
                (tie-route (second (first (select (fn [edge] (= start (first edge))) route)))
                           route))))

;; Processing routes in list manner is non trivial aim for any kind of graph. We become at this
;; point use follow approach. At any state of evaluation we can get some part of some number of
;; routes. At each step we can expand our sub routes on one edge. If concerned route has end point
;; in it, then we don't expand it any more. Evaluation of all simple routes list stops when we find
;; all routes to the end vertex and other final vertex doesn't has any childs which can bring us to
;; the end vertex.
;;
;; In two functions below we use `sub-routes' definition. It is a list of routes or rather parts of
;; routes. In simple manner it is list of sets of edges (two element vector of graph ids). Expand
;; route is action of adding one edge into each route in this list.

(defn expand-route-tail
  "This function accept list of route and return lisp of route which was expanded from tail. If
  complete route from start to end vertex is present in the list, then it will be unchanged."
  [start end sub-routes graph]
  (concat
   (mapcat
    (fn [route]
      (let [parent (route-end start route)]
        ;; This function create route list from list of new end vertices.
        (map
         (fn [child]
           (conj route (vector parent child)))
         ;; It's a list of edges spread from current end vertex to the new.
         (relation-childs
          parent
          graph))))
    ;; We don't expand sub-routes which is route to end point already.
    (filter
     (fn [sub-route]
       (not (route? start end sub-route)))
     sub-routes))
   ;; We add routes with final vertex to the processed list above.
   (filter
    (fn [sub-route]
      (route? start end sub-route))
    sub-routes)))

(defn expand-route-tail-rec
  "This function is a recursive version of `expand-route-list' which will stop evaluation on empty
  list of childs of not end points in the list of routes."
  [start end sub-routes graph]
  (if (empty?
         ;; List of all childs of end vertices.
         (mapcat
          (fn [route]
            (relation-childs (route-end start route) graph))
          (filter
           (fn [sub-route]
             (not (route? start end sub-route)))
           sub-routes)))
    sub-routes
    (recur start end (expand-route-tail start end sub-routes graph) graph)))

(defn route-list
  "Return list of all possible routes from parent to child in graph."
  [start end graph]
  (filter
   (fn [edges]
     (route? start end edges))
   (expand-route-tail-rec start
                          end
                          ;; Select list with first elements of future routes.
                          (map (fn [item]
                                 (set (list item)))
                               (select (fn [edge]
                                         (= (first edge) start))
                                       (second graph)))
                          graph)))

;; Obvious edges is some kind of parasite edges which make our graph parent child relation
;; ambiguous. For example, if we have to route from vertex A to the vertex B - one treat throw some
;; number of intermediate vertices and one is direct A -> B relation. So for any kind of generators
;; in future we must give the possibility of cleaning up simplest routes, because generator can lose
;; some critical particular from the all scene.

(defn delete-obvious-edges
  "Return clear graph without obvious direct relations between objects. For example for vertex sequence
  expressed through A -> B -> C and direct A -> C edges last will deleted from graph."
  [graph]
  (vector (vertices graph)
          (select (fn [[parent child]]
                    (= (list #{[parent child]})
                       (route-list parent child graph)))
                  (edges graph))))
