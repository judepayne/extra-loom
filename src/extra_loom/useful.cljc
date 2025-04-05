(ns extra-loom.useful
  (:require
   [tool-belt.core
    :refer [in? not-in? apply-to-if join update-in-all dissoc-in dissoc-in-when
            dissoc-in-clean update-in-all-if deep-merge-with]]
   [loom.graph :as lg]
   [loom.alg :as alg]
   [loom.attr :as at]
   [dictim.graph.core :as dg]
   [dictim.template :as tmp]
   [extra-loom.multigraph :as mg]
   [clojure.set :as s]))


(defn graph?
  "Returns true if g is a loom graph."
  [g]
  (satisfies? lg/Graph g))


(defn directed?
  "Returns true if g is directed."
  [g]
  (satisfies? lg/Digraph g))


(defn- digraph-tree?
  "Is g a digraph and a tree?"
  [g]
  (every?
   #(< (count (lg/predecessors g %)) 2)
   (lg/nodes g)))


(defn tree?
  "Is g a tree?"
  [g]
  (and
   (directed? g)
   (digraph-tree? g)))


(defn ancestors*
  "Returns all ancestors of the node in the graph."
  [g node]
  (letfn [(up [nd acc]
            (let [parents (into #{} (lg/predecessors g nd))]
              (if (seq parents)
                (mapcat #(up % (conj acc parents)) parents)
                acc)))]
    (reduce clojure.set/union (up node #{}))))


(defn roots
  "Returns the root nodes of the graph."
  [g]
  (filter #(not (seq (lg/predecessors g %))) (lg/nodes g)))


(defn root?
  "Returns true if the node is a root of the graph."
  [g n]
  (and (some (into #{n}) (lg/nodes g))
       (nil? (lg/predecessors g n))))


(defn kahn-sort
  "Returns a topological sort for directed Loom graph g.
   Removes cycles."
  ([g]
   (kahn-sort g []))
  ([g s]
   (let [rts (roots g)]
     (if (seq rts)
       (kahn-sort (apply lg/remove-nodes g rts) (concat s rts))
       s))))


(defn leaf?
  "Returns true if the node is a leaf in the graph."
  [g n]
  (nil? (lg/successors g n)))


(defn leaves
  "Returns the leaf nodes of the graph."
  [g]
  (filter (partial leaf? g) (lg/nodes g)))


(defn cycles?
  "Returns true if the graph contains cycles."
  [g]
  (not (boolean (alg/topsort g))))


;; need
;; 1. for madrkdown, cannot use :label. need a new node->label fn
;; 2. supply node->cluster, need to remap edges so dest is the cluster
;;    - I can do this already by comp'ing lg/dest & node->cluster


(defn graph->dictim
  "Converts a graph to dictim."
  [graph
   & {:keys [node-key
             node-attrs->dictim-attrs
             edge-attrs->dictim-attrs
             node->cluster
             cluster->parent
             cluster->attrs
             edge->src-key
             edge->dest-key
             template
             directives]
      :or {node-key nil
           node-attrs->dictim-attrs (constantly nil)
           edge-attrs->dictim-attrs (constantly nil)
           node->cluster (constantly nil)
           cluster->parent (constantly nil)
           cluster->attrs (constantly nil)
           edge->src-key lg/src
           edge->dest-key lg/dest
           template nil
           directives nil}
      :as opts}]
  
  (let [nodes (lg/nodes graph)
        dict (dg/graph->dictim nodes
                               (lg/edges graph)
                               {:node->key identity
                                :node->attrs (if node-key
                                               (fn [node] (node-attrs->dictim-attrs
                                                           (merge
                                                            {node-key node}
                                                            (at/attrs graph node))))
                                               (fn [node] (node-attrs->dictim-attrs (at/attrs graph node))))
                                :edge->attrs (fn [edge] (edge-attrs->dictim-attrs (at/attrs graph edge)))
                                :edge->src-key edge->src-key
                                :edge->dest-key edge->dest-key
                                :node->cluster node->cluster
                                :cluster->parent cluster->parent
                                :cluster->attrs cluster->attrs
                                :directives directives})]
    (if template (tmp/apply-template dict {:template template
                                           :directives directives
                                           :merge? true})
        dict)))


(defn distinct-edges
  "The distinct edges in the graph. eliminates mirrored edges."
  [g]
  (filter (complement :mirrored?) (lg/edges g)))


(defn pprint-graph
  "Pretty print a multidigraph. node-fn is a function that transform a node for display. Similarity
  edge-fn for an edge."
  ([g] (pprint-graph g identity identity identity))
  ([g node-fn edge-fn attr-fn]
   (println "Multidigraph")
   (println (count (lg/nodes g)) "Nodes:")
   (doseq [node (lg/nodes g)]
     (println \tab (node-fn node) (let [a (at/attrs g node)] (if (seq a) (attr-fn a) ""))))
   (println (count (lg/edges g)) "Edges:")
   (doseq [edge (lg/edges g)]
     (println \tab (edge-fn edge)
              (let [a (at/attrs g edge)]
                (if (seq a) a ""))))))


(defn pprint-graph2
  "a standard printing of a graph"
  [g]
  (pprint-graph g
                identity
                (fn [e]
                  (let [s (lg/src e)
                        d (lg/dest e)
                        m (mg/mirrored? e)]
                    (str s
                         (if m " <-> " " -> ")
                         d " ")))
                identity))


(defn referential-integrity?
  "Returns true if all src's & dest's in edges are contained in nodes.
  nodes must be specified as maps whose unique id is specified by node-key."
  [nodes node-key edges]
  (let [nds (into #{} (map node-key nodes))
        es (flatten (map (juxt lg/src lg/dest) edges))]
    (every? #(contains? nds %) es)))


(defn- merge-fn [& ns]
  (if (every? coll? ns)
    (apply s/union ns)
    (last ns)))


(defn merge-graphs
  "Merges multigraphs or multidigraphs."
  [& gs]
  (reduce (partial deep-merge-with merge-fn) gs))


(defn prewalk-tree
  "Prewalks the tree (specified as a loom graph), starting at node.
   f is a function is the current graph, a node's parent and the node itself
   that must return the new attrs to be assigned to the node.
   Returns the edited tree."
  [tree node f]
  {:pre [(tree? tree)]}
  (letfn [(down [g parent node]
            (let [new-attrs (f g parent node)
                  g (mg/add-attrs g node new-attrs)]
              (if-let [succs (lg/successors g node)]
                (reduce
                 (fn [acc cur]
                   (mg/add-attrs (down acc node cur) node new-attrs))
                 g
                 succs)
                g)))]
    (down tree node node)))


(defn prewalk-attrs
  "Prewalks the tree starting at node, applying f to the attrs of the node
   and each of its successors, recursively. Returns the updated tree.
   If node is not provided as the keyword argument `:node`, starts at
   the tree's root.
   The keyword argument `:consumes-edge-attrs?` determines if f is a 2-arity
   fn (the parent node's attributes and the current node's) or a 3-arity fn
   - as above but the third argument being the attrs of the edge between the
   two nodes."
  [tree f & {:keys [consumes-edge-attrs? node]
               :or {consumes-edge-attrs? false node (first (roots tree))}}]
  (prewalk-tree tree
                node
                (if (not consumes-edge-attrs?)
                  
                  (fn [g parent node]
                    (f (at/attrs g parent) (at/attrs g node)))

                  (fn [g parent node]
                    (f (at/attrs g parent)
                       (at/attrs g node)
                       (second (first (at/attrs g parent node))))))))


(defn postwalk-tree
  "Postwalks the tree (specified as a loom graph), starting at node.
   f is a function of the current graph, the current node's attrs
   and a seq of the attrs of its child nodes, and should return
   the new attrs to be assigned to the node.
   Returns the edited tree."
  [tree node f]
  {:pre [(tree? tree)]}
  (if-let [succs (lg/successors tree node)]
    (let [tree' (reduce (fn [a c] (postwalk-tree a c f)) tree succs)]
      (mg/add-attrs
       tree'
       node
       (f tree' node succs)))
    tree))


(defn postwalk-attrs
  "Postwalks the tree starting at node, applying f to the attrs of node
   and a sequence of the attrs of its children. Returns the updated tree.
   If node is not provided as the keyword argument `:node`, starts at the
   tree's root.
   The keyword argument `:consumes-edge-attrs?` determines if the second
   argument passed to f is a sequence of the child nodes' attrs or a
   sequence of maps of form
   `{:succ-attrs ..<the child node's attrs>
     :edges-attrs ..<the attrs of the edge between the two>}`"
  [tree f & {:keys [consumes-edge-attrs? node]
               :or {consumes-edge-attrs? false node (first (roots tree))}}]
  {:pre [(tree? tree)]}
  (postwalk-tree tree node
                 (if (not consumes-edge-attrs?)
                   
                   (fn [g node succs]
                     (f (at/attrs g node) (map (partial at/attrs g) succs)))
                   
                   (fn [g node succs]
                     (f (at/attrs g node)
                        (reduce
                         (fn [acc s]
                           (conj acc {:succ-attrs (at/attrs g s)
                                      :edge-attrs (second (first (at/attrs g node s)))}))
                         nil
                         succs))))))
