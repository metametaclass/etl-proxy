;; Copyright (c) 2012  Malyshev Artem  <proofit404@gmail.com>

(ns etl-proxy.json.generator
  (:use cheshire.core
        [etl-proxy.graph define comparison crud]
         clojure.tools.logging))

;; ## Generate JSON mark up expression from graph data structure.
;;
;; As we see in json parser module main approach in graph conversion process is a implementation of
;; foreign markup expression into single vertex. Then we need to divide this "complex" vertex into
;; "simple" vertex set. Generation is opposite task for parsing of JSON markup document. So if we
;; have large graph where parent child relation carry out into edges set, then we need to compose
;; few binded vertices into one large "complex" vertex which body structure will contain parent
;; child relation expressed throw json markup part. So when we will have graph with unique vertex
;; and empty edges set we can approve that we has json expression.
;;
;; ## Vertices composition rules.
;;
;; So instead of vertex body type we need analyze vertex child relation structure. And at this point
;; we can to declare rules set below to express final json document throw finite number or its
;; application.
;;
;; - vertex with empty child list is a final json composition. If this vertex is a vector of form
;;   like ['(key1 value1) ... '(keyN valueN)], then it must be expressed into map bellow
;;   {:key1 value1, ..., :keyN valueN}. I call this map rule.
;; - all series must be composed into single list vertex. For example, if we have graph like A -> B
;;   -> C -> D, A -> E -> F -> D, A -> G -> H -> D then applying current rule to B, E and G vertex
;;   will result as A -> '(B C) -> D, A -> '(E F) -> D, A -> '(G H) -> D correspondingly.
;; - if few vertices has equal lists of parents and children, then they must be conjunct into one
;;   vector. For example, if we has some graph like A -> B -> E, A -> C -> E, A -> D -> E, then
;;   current rule application will result into A -> [B C D] -> E graph.
;;

(defn map-comprehension?
  "This function return true in body of accepted vertex can be converted into map."
  [id graph]
  (let [body (body-by-id id graph)]
    (and
     ;; Vertex must be a vector.
     (vector? body)
     ;; I add one true value to list to be sure that all elements of list will be true.
     ;; We can't use (apply and ... because `and' has laziness as it permanent property.
     (apply = (concat '(true)
                      ;; Check that all items of vector are lists of two elements.
                      (map #(and (list? %) (= 2 (count %)))
                           body))))))

(defmulti compose-json
  "This function return new graph where accepted vertex will contain json map expressed from his
  parent-child relations."
  (fn [id graph]
    (let [rule (cond
                (map-comprehension? id graph) :map-rule
                (level-topology? id graph)    :vector-rule
                (series-member? id graph)     :series-rule
                :else                         false)]
      (debug "Id:" id "Applying" rule "rule.")
      rule)))

(defn final-json?
  "This function return true if there is a tuple of key and value represented as single list in the
  graph."
  [graph]
  (and (= 1 (count (vertices graph)))
       (list? (first (bodies graph)))
       (= 2 (count (first (bodies graph))))))

(defmethod compose-json false
  [id graph]
  ;; If graph contain only tuple of key and value as single list, then it must be resulted into
  ;; hash-map.
  (if (final-json? graph)
    ;; Wrap its body into vector and process as map rule.
    (let [new-graph (add-vertex [(first (bodies graph))] empty-graph)]
      (compose-json (first (ids new-graph)) new-graph))
    graph))

(defmethod compose-json :vector-rule
  [id graph]
  (let [;; Build body of new vertex from level ids and conjunct it into graph.
        level-as-single (apply vector (map #(body-by-id % graph) (get-level id graph)))
        new-graph (add-vertex level-as-single graph)
        single-id (id-by-body level-as-single new-graph)
        ;; Build new relations logically equals to old level.
        bind-to-top (map (fn [parent] (vector parent single-id)) (relation-parents id graph))
        bind-to-bottom (map (fn [child] (vector single-id child)) (relation-children id graph))]
    ;; Add all parent-child relations to new vertex.
    (add-edges-list (concat bind-to-top bind-to-bottom)
                    ;; Remove whole level from graph.
                    (delete-vertices-list (get-level id graph) new-graph))))

(defmethod compose-json :series-rule
  [id graph]
  (let [;; Create body for new element and add it to graph.
        series (get-series id graph)
        series-as-single (map #(body-by-id % graph) series)
        new-graph (add-vertex series-as-single graph)
        single-id (id-by-body series-as-single new-graph)
        ;; Build relations for new element.
        bind-to-top (map (fn [parent] (vector parent single-id)) (relation-parents (first series) graph))
        bind-to-bottom (map (fn [child] (vector single-id child)) (relation-children (last series) graph))]
    ;; Add all parent-child relations to new vertex.
    (add-edges-list (concat bind-to-top bind-to-bottom)
                    ;; Remove whole series from graph.
                    (delete-vertices-list series new-graph))))

(defmethod compose-json :map-rule
  [id graph]
  (let [;; Convert vector of tuple lists into map.
        new-body (apply merge (map #(apply hash-map %) (body-by-id id graph)))
        ;; Build in new vertex.
        new-graph (add-vertex new-body graph)
        new-id (id-by-body new-body new-graph)
        bind-to-top (map (fn [parent] (vector parent new-id)) (relation-parents id graph))
        bind-to-bottom (map (fn [child] (vector new-id child)) (relation-children id graph))]
    ;; Replace origin id with new map.
    (add-edges-list (concat bind-to-top bind-to-bottom)
                    ;; Remove whole series from graph.
                    (delete-vertex id new-graph))))

;; ## Down-Top processing approach.
;;
;; TODO: write documentation here.

(defn json-expression?
  "This function return true when given graph contain unique vertex with json expression."
  [graph]
  (and (= 1 (count (vertices graph)))
       (map? (first (bodies graph)))))

(defn json-appearance
  "This function return list of ids of vertices witch must be processed in some rule matter."
  [graph]
  ;; It necessary use two separate filters and join its together to save appropriate order for rules
  ;; application to vertex sequence.
  (concat
   (filter
    #(map-comprehension? % graph)
    (ids graph))
   ;; We also get only first members of series and levels because after its processing all ids of
   ;; level of series member will not exist anymore.
   (filter
    #(or (and (level-topology? % graph)
              (= % (apply min (get-level % graph))))
         (and (series-member? % graph)
              (= % (apply min (get-series % graph)))))
    ;; Instead of all graphs ids we must only to check those who don't refer to the previous
    ;; category.
    (filter
     #(not (map-comprehension? % graph))
     (ids graph)))
   ;; This filter represent final graph action. All actions above will empty and ids return only one
   ;; vertex. Otherwise this predicate will be empty.
   (filter
    (fn [nop] (final-json? graph))
    (ids graph))))

(defn graph2json
  "This function return json expression as string which correspond to given graph in topologically
  manner."
  [graph]
  ((fn [graph-state]
     (debug "------------------------------------------------------------")
     (debug graph-state)
     (debug (json-expression? graph-state))
     (debug (json-appearance graph-state))
     (debug "------------------------------------------------------------")
     (if (json-expression? graph-state)
       graph-state
       (recur ((list-action-on-graph compose-json)
               (json-appearance graph-state)
               graph-state))))
   graph)
  ;; (generate-string
  ;;  ;; Take unique vertex body as json expression.
  ;;  (first
  ;;   (bodies
  ;;    )))
  )
