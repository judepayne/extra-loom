(ns extra-loom.graph-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
            [loom.graph :as loom]
            [loom.attr :as attr]
            [extra-loom.graph :as gr]))


(defn empty-g [] (gr/multidigraph))


(defn idedges->vec [es] (map (juxt loom/src loom/dest) es))


(deftest Graph-protocol

  (testing "adding nodes"
    (is (= (-> (empty-g)
               (loom/add-nodes* [1])
               loom/nodes)
           #{1})))

  (testing "adding an edge"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2]]))]
      (is (= #{[1 2]}
             (into #{} (->> (loom/edges g) (map (juxt loom/src loom/dest))))))))
  
  (testing "adding two identical edges"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (and
           (= 2 (count (loom/edges g)))
           (true? (loom/has-edge? g 1 2))))))

;; mainly multi-edge 'multidigraph' tests from here on

  (testing "out degree with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= 2 (loom/out-degree g 1)))))

  (testing "successors with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= '(2) (loom/successors g 1)))))

  (testing "out-edges with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= '([1 2] [1 2]) (idedges->vec (loom/out-edges g 1)))))))


(deftest Digraph-protocol
  (testing "predecessors* with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= '(1) (loom/predecessors* g 2)))))


  (testing "in-degree* with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= 2 (loom/in-degree g 2)))))

  (testing "in-edges with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= '([1 2] [1 2] [0 2]) (idedges->vec (loom/in-edges g 2))))))

  (testing "transpose with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= g (-> g loom/transpose loom/transpose))))))


(deftest EditableGraph-protocol
  (testing "remove-nodes* with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= #{0 2}
             (loom/nodes (loom/remove-nodes* g [1]))))))

  (testing "remove-nodes leaving empties in :adj with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= [[0 2]]
             (idedges->vec (loom/edges (loom/remove-nodes* g [1])))))))

  (testing "remove-nodes leaving empties in :adj with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= [[0 2]]
             (idedges->vec (loom/edges (loom/remove-edges* g [[1 2]]))))))))


(deftest Building-a-graph
  (testing "building a graph & edges are right"
    (let [g (gr/multidigraph [1 2] [1 2] [0 2])]
      (is (= (idedges->vec (loom/edges g))
             [[1 2] [1 2] [0 2]]))))

  (testing "building a graph & the nodes are right"
    (let [g (gr/multidigraph [1 2] [1 2] [0 2])]
      (is (= (loom/nodes g)
             #{0 1 2}))))

  (testing "building a graph & the attrs are right"
    (let [g (gr/multidigraph [1 2] [1 2 {:color "white"}]
                             (with-meta {:a 1} {:attrs {:color "black"}}))]
      (is (= #{{:color "white"} {:color "black"}}
             (-> g :attrs vals set))))))


(deftest AttrGraph-protocol
  (testing "an attr from build can be fetched"
    (let [g (gr/multidigraph [1 2] [1 2] [0 2] (with-meta {:a 1} {:attrs {:color "black"}}) )]
      (is (= "black"
             (attr/attr g {:a 1} :color)))))

  (testing "an attr from build can be fetched"
    (let [g (gr/multidigraph [1 2] [1 2] [0 2] (with-meta {:a 1} {:attrs {:color "black"}}) )]
      (is (= {:color "black"}
             (attr/attrs g {:a 1})))))

  (testing "an attr can be added to a node after build and fetched"
    (let [g (gr/multidigraph [1 2] [1 2] [0 2] 4)]
      (is (= "black"
             (attr/attr (attr/add-attr g 4 :color "black") 4 :color)))))

  (testing "an attr can be added to a edge during build and fetched"
    (let [g (gr/multidigraph [1 2 {:color "black"}] [1 2] [0 2] 4)]
      (is (= "black"
             (val (first (attr/attr g 1 2 :color)))))))

  (testing "specifying an edge as a vector gives a map of attrs"
    (let [g (gr/multidigraph [1 2 {:color "black"}] [1 2 {:color "red"}] [0 2] 4)]
      (is (map? (attr/attr g 1 2 :color)))))

  (testing "specifying an edge as a vector gives a map of attrs of the right order"
    (let [g (gr/multidigraph [1 2 {:color "black"}] [1 2 {:color "red"}] [0 2] 4)]
      (is (= 2
             (count (attr/attr g 1 2 :color)))))))


#?(:cljs (cljs.test/run-tests))
