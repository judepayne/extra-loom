(ns extra-loom.graph
  (:require [extra-loom.utils
             :refer [in? not-in? apply-if join update-in-all dissoc-in dissoc-in-when dissoc-in-clean]]
            [loom.graph :refer [Graph ;; the Graph protocol & its members
                                nodes edges has-node? has-edge? successors* out-degree out-edges

                                Digraph ;; the Digraph protocol & its members
                                predecessors* in-degree in-edges transpose

                                EditableGraph ;; the EditableGraph protocol& members
                                add-nodes* add-edges* remove-nodes* remove-edges* remove-all

                                Edge
                                src dest

                                ;; any more vars needed by our implementation
                                successors predecessors]]
            [loom.graph :as lg]
            [loom.attr  :refer [AttrGraph ;; the AttrGraph protocol & its members
                                add-attr remove-attr attr attrs]]))

;; Edge

(defprotocol Identified
  (id [this] "Returns the id of this."))


;; Record for an Edge that implements Loom's Edge protocol, and helper fns
(defrecord UniqueEdge [id src dest])


(extend-type UniqueEdge
  lg/Edge
  (src [edge] (:src edge))
  (dest [edge] (:dest edge))
  Identified
  (id [edge] (:id edge)))


(defn make-edge
  "Returns a new UniqueEdge instance from src and dest."
  [src dest]
  (UniqueEdge.
   #?(:clj (java.util.UUID/randomUUID)
      :cljs (random-uuid))
   src dest))


(defn edge-equiv?
  "Does this edge have the same src and dest as the other?"
  [this other]
  (and (= (src this) (src other))
       (= (dest this) (dest other))))


(defn unique-edge?
  "Is e a UniqueEdge?"
  [e]
  (instance? UniqueEdge e))


(defn edge?
  "Is e an edge?"
  [e]
  (or (unique-edge? e)
      (and (instance? #?(:clj clojure.lang.IPersistentVector
                        :cljs cljs.core.PersistentVector) e)
           (> (count e) 1))))


;; Graph

;; A protocol for additional utilities required by MultipleEdges (between the same two nodes).
(defprotocol MultipleEdge
  (edges-between [g n1 n2] "Returns the edges in g between two nodes."))


(defn edges-between* [g n1 n2] (get-in g [:nodemap n1 :out-edges n2]))


;; A MultiEdge Editable Digraph allows for multiple edges between the same two nodes.
(defrecord MultiEdgeEditableDigraph [nodemap attrs])


(defn- has-node?*
  "Is node in g?"
  [g node]
  (if (get-in g [:nodemap node]) true false))


(defn- edges*
  "Returns the edges of the graph."
  [g]
  (reduce
   (fn [acc [k v]] (concat acc (-> v :out-edges vals join)))
   #{}
   (:nodemap g)))


(defn- nodes*
  "Returns the nodes of the graph."
  [g]
  (into #{} (keys (:nodemap g))))


;; EditableGraph

(defn- ins
  "Returns the in-edges of the node."
  [g node]
  (reduce
   (fn [acc [k v]]
     (if (in? (keys (:out-edges v)) node)
       (assoc acc k (get-in v [:out-edges node]))
       acc))
   {}
   (:nodemap g)))


(defn- add-in-edges
  "Adds the in-edges of the node to the node."
  [g node]
  (assoc-in g [:nodemap node :in-edges] (ins g node)))


(defn- add-node
  "Adds the node to the graph."
  [g node]
  (let [am (:attrs (meta node))
        g (-> g
              (update-in [:nodemap node :out-edges] merge {})
              (update-in [:nodemap node] assoc :in-edges (ins g node)))]
    (if am (update-in g [:attrs] assoc node am) g)))


(defn- add-edge
  "Adds an edge to the graph. The edge should be either a 2-vector [src dest]
  or a 3-vector [src dest attr-map]."
  [g edge]
  (let [[s d am] edge   ;; destructure into src dest attrp-map
        e (make-edge s d)
        ins (ins g s)
        g (-> g
              (add-node s)   ;; in this form, node s has no metadata/attrs
              (update-in [:nodemap s :out-edges d] (fnil conj #{}) e)
              (add-node d))] ;; in this form, node d has no metadata/attrs
        (if am (assoc-in g [:attrs (:id e)] am) g)))


(defn- excise-edge
  [m k edges-after]
  (if (empty? edges-after)
    (dissoc m k)
    (assoc m k edges-after)))


(defn- remove-unique-edge
  "Removes a unique edge from the graph."
  [g e]
  (let [{s :src d :dest id :id} e
        out-edges-after (disj (edges-between* g s d) e)
        in-edges-after (disj (edges-between* g d s) e)]
    (-> g
        (update-in [:nodemap s :out-edges] excise-edge d out-edges-after)
        (update-in [:nodemap d :in-edges] excise-edge s in-edges-after)
        (update-in [:attrs] dissoc id))))


(defn- remove-edge
  "Removes the edge from the graph."
  [g e]
  (let [edges (cond
                (unique-edge? e) (list e)
                :else (->> (edges-between* g (src e) (dest e))
                           (filter #(edge-equiv? e %))))]
    (reduce remove-unique-edge g edges)))


(defn- remove-node
  "Removes the node from the graph."
  [g node]
  (-> g
      (dissoc-in [:nodemap node])
      (dissoc-in [:attrs node])
      ;; remove orphaned in-edges
      (update-in-all [:nodemap :all :in-edges] dissoc node)))


(defn- remove-empty-edge-containers
  "Removes any empty entries in in-edges and out-edges. clean up"
  [g]
  (-> g
      (dissoc-in-when [:nodemap :all :out-edges :all] empty?)
      (dissoc-in-when [:nodemap :all :in-edges :all] empty?)))


;; these 3 functions are used to transpose
(defn- reverse-edge
  "Reverses the direction of an edge."
  [e]
  (assoc e :src (dest e) :dest (src e)))


(defn- reverse-edges
  "Reverses all the edges in a map of :out-edges or :in-edges"
  [m]
  (reduce
   (fn [acc [k v]] (assoc acc k (into #{} (map reverse-edge v))))
   {}
   m))


(defn- swap-ins-outs
  "Swaps in-edges with out-edges and reverse the direction of each edge in both."
  [m]
  (assoc m
         :out-edges (reverse-edges (:in-edges m))
         :in-edges (reverse-edges (:out-edges m))))


;; attrs

(defn- add-attr-to-edge
  "Adds an attr to an edge in the graph."
  ([g edge k v]
   (assoc-in g [:attrs (id edge) k] v))
  ([g n1 n2 k v]
   (reduce #(add-attr-to-edge %1 %2 k v) g (edges-between g n1 n2))))


(defn- add-attr-to-node
  "Adds an attr to the node in the graph."
  [g node k v]
  (assoc-in g [:attrs node k] v))


(defn- remove-attr-from-edge
  "Removes an attr from the edge in the graph."
  ([g edge k]
   (dissoc-in-clean g [:attrs (id edge) k]))
  ([g n1 n2 k]
   (reduce #(remove-attr-from-edge %1 %2 k) g (edges-between g n1 n2))))


(defn- remove-attr-from-node
  "Removes an attr from the node in the graph."
  [g node k]
  (dissoc-in-clean g [:attrs node k]))


(defn- attr*
  "Returns the attr with key k or a map of values for the node or edge in the graph.
  A map of values {edge-id attr) is returned when the edge is specified as n1 n2
  and so is (potentially) ambiguous."
  ([g node-or-edge k]
   (cond
     (unique-edge? node-or-edge) (get-in g [:attrs (id node-or-edge) k])
     :else                       (get-in g [:attrs node-or-edge k])))
  ([g n1 n2 k]
   (let [es (edges-between g n1 n2)]
     (reduce
      (fn [ats edge]
        (let [at (attr* g edge k)]
          (if at (assoc ats (id edge) at) ats)))
      nil
      es))))


(defn- attrs*
  "Returns the attrs or a map of values for the node or edge in the graph.
  A map of values {edge-id attrs) is returned when the edge is specified as n1 n2
  and so is (potentially) ambiguous."
  ([g node-or-edge]
   (cond
     (unique-edge? node-or-edge) (get-in g [:attrs (id node-or-edge)])
     :else                       (get-in g [:attrs node-or-edge])))
  ([g n1 n2]
   (let [es (edges-between g n1 n2)]
     (reduce
      (fn [atts edge]
        (let [ats (attrs* g edge)]
          (if ats (assoc atts (id edge) ats) atts)))
      nil
      es))))


(extend-type MultiEdgeEditableDigraph

  Graph
  (nodes [g] (nodes* g))
  (edges [g] (edges* g))
  (has-node? [g node] (contains? (nodes g) node))
  (has-edge? [g n1 n2] (let [m (get-in g [:nodemap n1 :out-edges])]
                         (if (some #{n2} (keys m)) true false)))
  (successors* [g node] (keys (get-in g [:nodemap node :out-edges])))
  (out-degree [g node] (count (join (vals (get-in g [:nodemap node :out-edges])))))
  (out-edges [g node] (join (vals (get-in g [:nodemap node :out-edges]))))

  Digraph
  (predecessors* [g node] (keys (get-in g [:nodemap node :in-edges])))
  (in-degree [g node] (count (join (vals (get-in g [:nodemap node :in-edges])))))
  (in-edges [g node] (join (vals (get-in g [:nodemap node :in-edges]))))     
  (transpose [g] (update-in-all g [:nodemap :all] swap-ins-outs))

  EditableGraph
  (add-nodes* [g nodes] (reduce add-node g nodes))
  (add-edges* [g edges] (reduce add-edge g edges))
  (remove-nodes* [g nodes] (reduce remove-node g nodes))
  (remove-edges* [g edges] (remove-empty-edge-containers (reduce remove-edge g edges)))
  (remove-all [g] (assoc g :nodemap {} :attrs {}))

  MultipleEdge
  (edges-between [g n1 n2] (edges-between* g n1 n2))

  AttrGraph
  ;; note the syntax for extend-type and multi-arity protocols.
  (add-attr
    ([g node-or-edge k v] (if (unique-edge? node-or-edge)
                            (add-attr-to-edge g node-or-edge k v)
                            (add-attr-to-node g node-or-edge k v)))
    ([g n1 n2 k v] (add-attr-to-edge g n1 n2 k v)))
  (remove-attr
    ([g node-or-edge k] (if (unique-edge? node-or-edge)
                          (remove-attr-from-edge g node-or-edge k)
                          (remove-attr-from-node g node-or-edge k)))
    ([g n1 n2 k] (remove-attr-from-edge g n1 n2 k)))
  (attr
    ([g node-or-edge k] (attr* g node-or-edge k))
    ([g n1 n2 k] (attr* g n1 n2 k)))
  (attrs
    ([g node-or-edge] (attrs* g node-or-edge))
    ([g n1 n2] (attrs* g n1 n2))))


;; building

(defn- build-graph
  "Builds a multidigraph from the sequence of edges-or-nodes."
  [edges-or-nodes]
  (reduce
   (fn [g cur]
     (if (edge? cur)
       (add-edge g cur)
       (add-node g cur)))
   {}
   edges-or-nodes))


(defn multidigraph [& edges-or-nodes]
  (let [g (build-graph edges-or-nodes)]
    (MultiEdgeEditableDigraph. (:nodemap g) (:attrs g))))


;; taken from Ubergraph
(defn pprint
  "Pretty print a multidigraph"
  [g]
  (println "Multidigraph")
  (println (count (nodes g)) "Nodes:")
  (doseq [node (nodes g)]
    (println \tab node (let [a (attrs g node)] (if (seq a) a ""))))
  (println (count (edges g)) "Edges:")
  (doseq [edge (edges g)]
    (println \tab (src edge) "->" (dest edge)
             (let [a (attrs g edge)]
               (if (seq a) a "")))))

 

;; in draw-graph....
;; Use of loom.alg-generic:
;; pre-edge-traverse bf-path

;; Use of loom.attr
;; -in graph.cljc


(comment
  (defn edge-invisible?
  [g n1 n2]
  (let [style (:style (loom.attr/attrs g n1 n2))]
    (and style (some #(= "invis" %) (str/split style #","))))))

(comment
  ;; n is a node
  (if (and (root? g opts n) (map? (loom.attr/attrs g n)))
     (dissoc (loom.attr/attrs g n) :shape)
     (loom.attr/attrs g n)))

(comment
  (defn ^:private constrained? [g n1 n2]
  (loom.attr/attr g n1 n2 :constraint)))

(comment
  (defn edge-label
  "Returns the label for the edge n1 n2 in g given options."
  [g opts n1 n2]
  (when-let [lbls (-> opts :edge :edge-label)]
    (let [metadata (loom.attr/attr g n1 n2 :meta)]
      (if (str/includes? lbls "/")
        (first-label lbls metadata)
        (composite-label lbls metadata))))))

(comment
  (defn ^:private edge-descriptor
  "Return map of attributes for the edge from *display-conf*"
  [g opts n1 n2]
  (merge
   (if (-> opts :edge :edge-label)
     {:xlabel (doub-slash-n (edge-label g opts n1 n2)) :forcelabels true}
     nil)
   (constraints g opts n1 n2)
   ;; per edge attrs supplied by user
   (dissoc (loom.attr/attrs g n1 n2) :meta)
   (maybe-show-constraint g opts n1 n2))))

;; -in processor.cljc
(comment
  (defn- add-attr-map
    [g node-or-edge m]
    (reduce
     (fn [acc cur] (apply loom.attr/add-attr acc node-or-edge cur))
     g (vec m))))

(comment
  (defn- add-meta-map-to-edge
  [g src dst m]
  (loom.attr/add-attr-to-edges g :meta m [[src dst]])))

;; -in preprocessor.cljc
(comment
  (loom.attr/add-attr-to-edges :style "invis" edges'-f))  ;; <-- multiple edges

(comment
  (not= (loom.attr/attr g' src dst :style) "invis"))

(comment
  (filter-fn (loom.attr/attr g' src dst :meta)))

;; - (attrs g n1 n2) getting all attrs for an edge specified with n1 n2
;; - (attrs g n) getting all attrs for a node
;; - (attr g n1 n2 k) getting an attrs for an edge specified with n1 n2 and key k
;; - (add-attr g node-or-edge attr-map) In a reduce, so adds map of attrs in one go
;; - (add-attr-to-edges k v edges) add-attr-to-edges. adds single attr to a sequential coll of edges

;; ideas
;; 1. metadata can be attached to a vector (an edge), but loom rather stores edges as :adj, :in so
;; no vec.
;; 2. 
