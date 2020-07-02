(ns extra-loom.useful
  (:require
   [tool-belt.core
    :refer [in? not-in? apply-to-if join update-in-all dissoc-in dissoc-in-when
            dissoc-in-clean update-in-all-if deep-merge-with]]
   [loom.graph :as lg]
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
