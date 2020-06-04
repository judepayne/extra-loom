# extra-loom

<img align="right" src="multiedge.png">

Clojure/ Clojurescript extension to the Loom graph library with graphs that allow multiple edges between the same nodes.
The graphs here conform to all Loom's protocols so the richness of Loom's libraries 'just work'.

Inspired by Ubergraph.
Deliberately keep simple for Clojurescript compatibility; Loom is the only dependency.

## Installation

    [extra-loom "0.1.0"]

## Usage

    (require '[extra-loom.graph :refer :all])


At present the only additional implemented graph is a `multidigraph`, a digraph that can have multiple edges between its nodes.


Let's define one:

    (def g (multidigraph [1 2] [2 3] [2 3] [1 2] [1 3 {:sugar true}] 4))

The arguments is a sequence of nodes (e.g. 4 in example above) or edges (a [src dest] vector) which may include an optional map of attrs in the third position in the vector.

Nodes can be given attr metadata during the definition step too. Please see the **Attrs** section below for details.
    
The multidigraph has its own type:

    (type g) ;; ==>  extra_loom.graph.MultiEdgeEditableDigraph
    
A multidigraph implements the following Loom protocols:

    - Graph
    - Digraph
    - EditableGraph
    - AttrGraph

It is not weighted.

To use call the functions in the implementations of those protocols, you require the `Loom.graph` or `loom.attr` namespaces:

    (require '[loom.graph :as l])

    (l/edges g)
    ;; ==>
    ({:id #uuid "efd71571-5861-4ef5-bcf0-d830e7010b88", :src 1, :dest 2}
     {:id #uuid "30f2885d-b878-4aa5-8012-d76090f00d14", :src 1, :dest 2}
     {:id #uuid "4dc7b2dd-a245-4190-a520-73c755aca47e", :src 1, :dest 3}
     {:id #uuid "99e4c402-5bc0-4e91-a653-58e0b1880132", :src 2, :dest 3}
     {:id #uuid "6b74efaa-01f6-45d3-8ed2-716990accfbb", :src 2, :dest 3})
     
The edges of a multidigraph have their own type, and are identified by a UUID:

    (type (first (l/edges g))) ;; ==> extra_loom.graph.UniqueEdge
    
A new type is required since Loom uses 2 vectors e.g. `[2 3]` which are not unique in the context of a graph that have multiple edges between the same two nodes.
UniqueEdge is compatible with the Loom `Edge` protocol:

    (l/src (first (l/edges g))) ;; ==> 1
    
    (l/dest (first (l/edges g))) ;;==> 2
    
The example graph `g` above has two edges between the nodes `1` and `2`.

    (l/out-edges g 1)
    ;; ==>
    ({:id #uuid "efd71571-5861-4ef5-bcf0-d830e7010b88", :src 1, :dest 2}
     {:id #uuid "30f2885d-b878-4aa5-8012-d76090f00d14", :src 1, :dest 2}
     {:id #uuid "4dc7b2dd-a245-4190-a520-73c755aca47e", :src 1, :dest 3})
     
The successors* of 1 are:

    (l/successors* g 1) ;; ==> (2 3)

As well as implementing the Loom protocols, extra-loom defines a protocol called `MultipleEdge` to hold useful functions for working with multiedges, for example `edges-between`:

    (edges-between g 1 2)
    ;; ==>
    #{{:id #uuid "935a8f13-7d83-4206-a01a-0c614301f260", :src 1, :dest 2}
      {:id #uuid "73900eba-fd5e-4983-aac8-17b48bcbd184", :src 1, :dest 2}}


### Graph Algorithms

Since multidigraph follows all the Loom protocols, Loom's algorithms 'just work':

    (require '[loom.alg-generic :as ag])
    
    (ag/pre-edge-traverse #(l/successors* g %) 1)
    ;; ==>
    ([1 2] [2 3] [1 3])

A longer example:

    user> (def edges [[1 2] [1 5] [2 3] [2 4] [3 6] [4 6] [4 8] [5 6] [7 2]])
    #'user/edges

    user> (def dig (apply l/digraph edges))            ;; A Loom digraph
    #'user/dig

    user> (def mdig (apply multidigraph edges))
    #'user/mdig

    user> (ag/bf-path #(l/successors* dig %) 1 6)
    (1 5 6)

    user> (ag/bf-path #(l/successors* mdig %) 1 6)
    (1 5 6)   ;; i.e. the same

    user> (def mdig0 (apply multidigraph (conj edges [2 4] [3 6]))) ;; two multiple edges
    #'user/mdig0

    user> (ag/bf-path #(l/successors* mdig0 %) 1 6)
    (1 5 6)   ;; i.e. *still* the same
  
**please note:** Not tested on *all* of Loom's algorithms. Please raise an issue if there's a problem.

### Attrs

We saw how edges can be given attr metadata during graph definition. So can nodes:

    (def g (multidigraph [1 2] [1 2] [1 3] (with-meta {:a 1} {:attrs {:color "black"}})))
    
In this case `{:a 1}` is a node that has been initialised with an attr-map `{:color "black"}`. Clojure doesn't allow for meta data to be added to all types, e.g.:

    (with-meta 4 {:attrs {:color "black"}})
    ;; ==>
    Execution error (ClassCastException) at user/eval8002 (REPL:178).
    class java.lang.Long cannot be cast to class clojure.lang.IObj
    
When your node is of a type that doesn't support metadata, attrs can be added by using Loom's `AttrGraph` protocol. For example:

    (add-attr g 4 :color "black")
    
will add {:color "black"} to the node 4.

attrs can be added to edges either by specifying a precise edge, e.g.

    (def es (edges g))
    ;; ==>
    ({:id #uuid "935a8f13-7d83-4206-a01a-0c614301f260", :src 1, :dest 2}
     {:id #uuid "73900eba-fd5e-4983-aac8-17b48bcbd184", :src 1, :dest 2}
     {:id #uuid "a1ebfad4-c9b9-480d-9847-3c66f9a28167", :src 1, :dest 3})
     
    (add-attr g (first es) :color "white")
    
or when an edge is specified by its `src` and `dest` (and so is ambiguous in the context of a multidigraph)

    (add-attr g 1 2 :color "white")
    
The attr would be added onto both edges between nodes 1 and 2.


### License

This software is distributed under the MIT license.
