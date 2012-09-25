;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph.comparison
  (:use etl-proxy.graph.crud))

;; ## Several graph analyse tool.
;;
;; The main aim of this module is provide simple function set for work with several things of the
;; same kind. If you need to define any function which must to analyse some vertices of the same
;; graph or some numbers of graphs, do it in this module.

(defn bodies-bound?
  "This function return true if there is edge between this bodies in the graph."
  [one other graph]
  ;; Both bodies must be members of graph, and
  (and (graph-member? one graph)
       (graph-member? other graph)
       (let [one-id (id-by-body one graph)
             other-id (id-by-body other graph)]
         ;; edge set of any parent-child relation between bodies must be nonempty.
         (not (empty? (edges-subset (list one-id other-id)
                                    (list one-id other-id)
                                    graph))))))

;; For some kind of our stuffs we maybe need adjacency matrix. So lets implement it in the way
;; hash-map which contain parents bodies as keys and sets of child's bodies as values in it.

(defn adjacency-map
  "This function return adjacency matrix for graph as hash-map."
  [graph]
  (into {}
        ;; Key-value vectors list.
        (map (fn [body]
               (vector body
                       ;; Set of childs bodies for current vertex.
                       (set (map (fn [child] (body-by-id child graph))
                                 (relation-childs (id-by-body body graph) graph)))))
             (bodies graph))))

;; For many reasons we need possibility of comparison two or more graphs. But we can't use direct
;; equality operator in this staff, because two equal graphs can have different indexation. This
;; doesn't break logical equality but represent the same things in different data structures. So
;; there are two main properties which must be realized for graphs equalities. First of all they all
;; must have the same bodies sets. And second thing which they must to realize, the same vertices
;; must be bound with same in others graphs. In other words their adjacency matrices must be the
;; same.

(defn graph=
  "Return true if all accepted graphs logically equal."
  [one & others]
  (and (apply = (map bodies (cons one others)))
       (apply = (map adjacency-map (cons one others)))))
