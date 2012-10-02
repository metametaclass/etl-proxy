;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.graph.comparison
  (:use [etl-proxy.graph define crud]))

;; ## Graph analyse tool.
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

;; ## Topology analyze.
;;
;; At this section we will define function set for check vertex scope type. First of all we define
;; level topology. In this situation few vertices occurs with equal set of parents and childs. Then
;; we say "Such vertices topology is a same level topology." For example B, C and D vertices occur
;; the same level topology in A -> B -> E, A -> C -> E, A -> D -> E graph. But there is no same
;; level in A -> B -> G -> E, A -> C -> E, A -> D -> W -> E graph.

(defn level-topology?
  "This function return true when given vertex is a part of any level topology in graph."
  [id graph]
  (let [level-parents (relation-parents id graph)
        level-childs (relation-childs id graph)
        level-from-top (map (fn [parent] (set (relation-childs parent graph))) level-parents)
        level-from-bottom (map (fn [child] (set (relation-parents child graph))) level-childs)]
    (and
     ;; All parents has same child level.
     (apply = level-from-top)
     ;; All childs has same parent level.
     (apply = level-from-bottom)
     ;; Levels above are equal.
     (= level-from-top level-from-bottom))))

(defn get-level)

;; Second topology at this section is a series topology. Series is an _N_ vertices bound with
;; _N_-_1_ edges in circuit manner. That is first vertex bound with second, second with third, etc.
;; For example A -> B -> C is a series, B-> C is a series in A -> B -> C -> E, A -> D -> E graph but
;; nothing else. Series contain at least two vertices.

(defn series-member?
  "This function return true if given vertex is a part of any series in graph."
  [id graph]
  (let [parents-list (relation-parents id graph)
        childs-list (relation-childs id graph)]
    (or
     ;; Check for series beginning.
     (and (= 1 (count parents-list))
          (= 1 (count (relation-childs (first parents-list) graph))))
     ;; Check for series end.
     (and (= 1 (count childs-list))
          (= 1 (count (relation-parents (first childs-list) graph)))))))

(defn get-series
  "This function return full series in which specified element occurs as list."
  [id graph]
  ;; Find all series elements by recursive rising and lowering parent-child relation until series
  ;; membership predicate is satisfied.
  (when (series-member? id graph)
    (let [;; Recursive rising.
          higher-list ((fn [parent ups]
                         (if (series-member? parent graph)
                           (recur (first (relation-parents parent graph)) (cons parent ups))
                           ups))
                       id (list))
          ;; Recursive lowering.
          lower-list ((fn [child downs]
                        (if (series-member? child graph)
                          (recur (first (relation-childs child graph)) (concat downs (list child)))
                          downs))
                      id (list))]
      ;; Exclude one source vertex.
      (concat higher-list (rest lower-list)))))
