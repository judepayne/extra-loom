(ns extra-loom.multigraph
  (:require
   [tool-belt.core
    :refer [in? not-in? apply-to-if join update-in-all dissoc-in dissoc-in-when
            dissoc-in-clean update-in-all-if deep-merge-with]]
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
                       add-attr remove-attr attr attrs]]
   #?@(:clj [[loom.cljs :refer (def-protocol-impls)]]))
            #?@(:cljs [(:require-macros [loom.cljs :refer [def-protocol-impls extend]])]))

;; ****** Edge ******

(defprotocol Identified
  (id [this] "Returns the id of this.")
  (mirrored? [this] "Returns true if this is a mirrored edge."))


;; Record for an Edge that implements Loom's Edge protocol, and helper fns
(defrecord UniqueEdge [id src dest mirrored?])


(extend-type UniqueEdge
  lg/Edge
  (src [edge] (:src edge))
  (dest [edge] (:dest edge))
  Identified
  (id [edge] (:id edge))
  (mirrored? [edge] (:mirrored? edge)))


(extend-type #?(:clj clojure.lang.IPersistentVector
                :cljs cljs.core.PersistentVector)
  Identified
  (id [this] nil)
  (mirrored? [this] false))


(defn make-edge
  "Returns a new UniqueEdge instance from src and dest."
  ([src dest]
   (UniqueEdge.
    #?(:clj (java.util.UUID/randomUUID)
       :cljs (random-uuid))
    src dest false))
  ([src dest mirrored?]
   (UniqueEdge.
    #?(:clj (java.util.UUID/randomUUID)
       :cljs (random-uuid))
    src dest mirrored?)))


(defn unique-edge?
  "Is e a UniqueEdge?"
  [e]
  (instance? UniqueEdge e))


(defn edge-equiv?
  "Does this edge have the same src and dest as the other?"
  [this other]
  (or
   (and (= (mirrored? this) (mirrored? other))
        (= (src this) (src other))
        (= (dest this) (dest other)))
   (and (not= (mirrored? this) (mirrored? other))
        (= (src this) (dest other))
        (= (dest this) (src other)))))


(defn edge?
  "Is e an edge?"
  [e]
  (or (unique-edge? e)
      (and (instance? #?(:clj clojure.lang.IPersistentVector
                        :cljs cljs.core.PersistentVector) e)
           (> (count e) 1))))


;; ****** Graph ******

;; A protocol for additional utilities required by MultipleEdges (between the same two nodes).
(defprotocol MultipleEdge
  (edges-between [g n1 n2] "Returns the edges in g between two nodes."))


(defn edges-between* [g n1 n2] (get-in g [:nodemap n1 :out-edges n2]))


(defn- edges-of-type
  "Fetches the edges in the graph of edge-type which should be either
   :in-edges or :out-edges."
  [g edge-type]
  (reduce
   (fn [acc [k v]] (concat acc (-> v edge-type vals join)))
   #{}
   (:nodemap g)))


;; A MultiEdge Graphs/ Digraphs allows for multiple edges between the same two nodes.
(defrecord MultiEdgeEditableDigraph [nodemap attrs])


(defrecord MultiEdgeEditableGraph [nodemap attrs])


(defn digraph?
  [g]
  (boolean (satisfies? Digraph g)))


(defn- has-node?*
  "Is node in g?"
  [g node]
  (boolean (get-in g [:nodemap node])))


(defn- edges*
  "Returns the edges of the graph."
  [g]
  (edges-of-type g :out-edges))


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
  (-> g
      (update-in [:nodemap node :out-edges] merge {})
      (add-in-edges node)))


(defn- add-nodes
  "Adds the nodes to the graph."
  [g & nodes] (reduce add-node g nodes))


(defn- reverse-edge
  "Reverses the direction of an edge."
  [e]
  (assoc e :src (dest e) :dest (src e)))


(defn- mirrored-edge
  "Creates a mirrored edge."
  [e]
  (assoc (reverse-edge e) :mirrored? true))


(defn- add-mirrored-edge
  "Creates a mirrored edge of e and adds to the graph."
  [g e]
  (let [s (src e)
        d (dest e)
        e-mirror (mirrored-edge e)]
    (-> g
        (update-in [:nodemap d :out-edges s] (fnil conj #{}) e-mirror)
        (update-in [:nodemap s :in-edges d] (fnil conj #{}) e-mirror))))


(defn- add-edge
  "Adds an edge to the graph. The edge should be either a 2-vector [src dest]
  or a 3-vector [src dest attr-map]. mirrored? indicates a mirrored edge in a graph, not digraph."
  [g edge & {:keys [mirrored?] :or {mirrored? false}}]
  (let [[s d am] edge   ;; destructure into src dest attrp-map
        e (make-edge s d)]      
        (-> g
              (update-in [:nodemap s :out-edges d] (fnil conj #{}) e)
              (apply-to-if mirrored? add-mirrored-edge e)
              (add-nodes s d) ;; to add d non-destructively & capture the in-edges for s & d.
              (apply-to-if am assoc-in [:attrs (:id e)] am))))


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
        (update-in [:nodemap s :in-edges] excise-edge d in-edges-after)
        (update-in [:nodemap d :out-edges] excise-edge s out-edges-after)
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
      (update-in-all [:nodemap :all :in-edges] dissoc node)
      (update-in-all [:nodemap :all :out-edges] dissoc node)))


(defn- remove-empty-edge-containers
  "Removes any empty entries in in-edges and out-edges. clean up"
  [g]
  (-> g
      (dissoc-in-when [:nodemap :all :out-edges :all] empty?)
      (dissoc-in-when [:nodemap :all :in-edges :all] empty?)))


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


;; Default implementations of the protocols
(def-protocol-impls ^:private impl-graph
  {:nodes (fn [g] (nodes* g))
   :edges (fn [g] (edges* g))
   :has-node? (fn [g node] (contains? (nodes* g) node))
   :has-edge? (fn [g n1 n2] (let [m (get-in g [:nodemap n1 :out-edges])] (boolean (some #{n2} (keys m)))))
   :successors* (fn [g node] (keys (get-in g [:nodemap node :out-edges])))
   :out-degree (fn [g node] (count (join (vals (get-in g [:nodemap node :out-edges])))))
   :out-edges (fn [g node] (join (vals (get-in g [:nodemap node :out-edges]))))})


(def-protocol-impls ^:private impl-digraph
  {:predecessors* (fn [g node] (keys (get-in g [:nodemap node :in-edges])))
   :in-degree (fn [g node] (count (join (vals (get-in g [:nodemap node :in-edges])))))
   :in-edges (fn [g node] (join (vals (get-in g [:nodemap node :in-edges]))))     
   :transpose (fn [g] (update-in-all g [:nodemap :all] swap-ins-outs))})


(def-protocol-impls ^:private impl-editablegraph
  {:add-nodes* (fn [g nodes] (apply add-nodes g nodes))
   :add-edges* (fn [g edges] (if (digraph? g)
                               (reduce add-edge g edges)
                               (reduce #(add-edge %1 %2 :mirrored? true) g edges)))
   :remove-nodes* (fn [g nodes] (reduce remove-node g nodes))
   :remove-edges* (fn [g edges] (remove-empty-edge-containers (reduce remove-edge g edges)))
   :remove-all (fn [g] (assoc g :nodemap {} :attrs {}))})


(def-protocol-impls ^:prviate impl-multipleedge
  {:edges-between (fn [g n1 n2] (edges-between* g n1 n2))})


(def-protocol-impls ^:private impl-attrgraph
  {:add-attr
   (fn
     ([g node-or-edge k v] (if (unique-edge? node-or-edge)
                               (add-attr-to-edge g node-or-edge k v)
                               (add-attr-to-node g node-or-edge k v)))
     ([g n1 n2 k v] (add-attr-to-edge g n1 n2 k v)))
   :remove-attr
   (fn
     ([g node-or-edge k] (if (unique-edge? node-or-edge)
                             (remove-attr-from-edge g node-or-edge k)
                             (remove-attr-from-node g node-or-edge k)))
     ([g n1 n2 k] (remove-attr-from-edge g n1 n2 k)))
   :attr
   (fn
     ([g node-or-edge k] (attr* g node-or-edge k))
     ([g n1 n2 k] (attr* g n1 n2 k)))
   :attrs
   (fn
     ([g node-or-edge] (attrs* g node-or-edge))
     ([g n1 n2] (attrs* g n1 n2)))})


;; ****** Public API ******

;; A multigraph
(extend MultiEdgeEditableGraph
  Graph
  impl-graph

  EditableGraph
  impl-editablegraph

  MultipleEdge
  impl-multipleedge

  AttrGraph
  impl-attrgraph)


;; A multidigraph
(extend MultiEdgeEditableDigraph
  Graph
  impl-graph

  Digraph
  impl-digraph

  EditableGraph
  impl-editablegraph

  MultipleEdge
  impl-multipleedge

  AttrGraph
  impl-attrgraph)


;; building

(defn build-graph
  "Builds up a graph (i.e. adds edges and nodes) from any combination of
  adjacency maps, edges, or nodes."
  [g & inits]
  (letfn [(build [g init]
            (cond
             ;; adacency map
             (map? init)
             (let [es (if (map? (val (first init)))
                        (for [[n nbrs] init
                              [nbr wt] nbrs]
                          [n nbr wt])
                        (for [[n nbrs] init
                              nbr nbrs]
                          [n nbr]))]
               (-> g
                   (add-nodes* (keys init))
                   (add-edges* es)))
             ;; edge
             (edge? init) (add-edges* g [init])
             ;; node
             :else (add-node g init)))]
    (reduce build g inits)))


(defn multigraph [& inits]
  (apply build-graph (MultiEdgeEditableGraph. {} {}) inits))


(defn multidigraph [& inits]
  (apply build-graph (MultiEdgeEditableDigraph. {} {}) inits))


(defn distinct-edges
  "The distinct edges in the graph. eliminates mirrored edges."
  [g]
  (filter (complement :mirrored?) (edges g)))


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


(defn merge-graphs
  "Merges multigraphs or multidigraphs."
  [& gs]
  (reduce (partial deep-merge-with clojure.set/union) gs))


(defn extra-loom-graph?
  "Returns true is g is an extra-loom graph."
  [g]
  (let [t (type g)]
    (or (= t extra_loom.multigraph.MultiEdgeEditableDigraph)
        (= t extra_loom.multigraph.MultiEdgeEditableGraph))))
