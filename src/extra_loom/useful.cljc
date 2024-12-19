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


(defn graph->dictim
  "Converts a graph to dictim."
  [graph
   & {:keys [node-attrs->dictim-attrs
             edge-attrs->dictim-attrs
             node->cluster
             cluster->parent
             cluster->attrs
             template
             directives]
      :or {node-attrs->dictim-attrs (constantly nil)
           edge-attrs->dictim-attrs (constantly nil)
           node->cluster (constantly nil)
           cluster->parent (constantly nil)
           cluster->attrs (constantly nil)
           template nil
           directives nil}
      :as opts}]
  
  (let [nodes (lg/nodes graph)
        dict (dg/graph->dictim nodes
                               (lg/edges graph)
                               {:node->key identity
                                :node->attrs (fn [node] (node-attrs->dictim-attrs (at/attrs graph node)))
                                :edge->attrs (fn [edge] (edge-attrs->dictim-attrs (at/attrs graph edge)))
                                :edge->src-key lg/src
                                :edge->dest-key lg/dest
                                :node->cluster node->cluster
                                :cluster->parent cluster->parent
                                :cluster->attrs cluster->attrs})]
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


(defn merge-graphs
  "Merges multigraphs or multidigraphs."
  [& gs]
  (reduce (partial deep-merge-with s/union) gs))
