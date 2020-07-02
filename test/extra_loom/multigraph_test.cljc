(ns extra-loom.multigraph-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
            [loom.graph :as loom]
            [loom.attr :as attr]
            [extra-loom.multigraph :as gr]))

(defn empty-g [] (gr/multigraph))


(defn empty-dg [] (gr/multidigraph))


(def relationships
  [[{:animal "pandas", :name "simone"}
    {:animal "pandas", :name "summer"}
    {:color "green2"}]
   [{:animal "pandas", :name "simone"}
    {:animal "pandas", :name "ivy"}
    {:color "green2"}]
   [{:animal "brownbears", :name "cosmo"}
    {:animal "brownbears", :name "eliza"}
    {:color "deeppink"}]
   [{:animal "pandas", :name "kacey"}
    {:animal "brownbears", :name "cosmo"}
    {:color "deeppink"}]
   [{:animal "pandas", :name "ivy"}
    {:animal "squirrels", :name "huxley"}
    {:color "red"}]
   [{:animal "pandas", :name "max"}
    {:animal "pandas", :name "Bridget"}
    {:color "blue"}]
   [{:animal "pandas", :name "malakai"}
    {:animal "pandas", :name "ivy"}
    nil]
   [{:animal "pandas", :name "kacey"}
    {:animal "pandas", :name "shahar"}
    {:meta {:relationship "friend"}}]
   [{:animal "pandas", :name "cristolene"}
    {:animal "pandas", :name "shahar"}
    {:color "deeppink", :meta {:relationship "friend"}}]
   [{:animal "pandas", :name "malakai"}
    {:animal "pandas", :name "delila"}
    nil]
   [{:animal "pandas", :name "nikkai"}
    {:animal "pandas", :name "isobel"}
    nil]
   [{:animal "pandas", :name "shahar"}
    {:animal "pandas", :name "summer"}
    nil]
   [{:animal "pandas", :name "simone"}
    {:animal "pandas", :name "max"}
    {:meta {:relationship "enemy"}}]
   [{:animal "pandas", :name "simone"}
    {:animal "pandas", :name "max"}
    {:meta {:relationship "friend"}}]])


(defn idedges->vec [es] (map (juxt loom/src loom/dest) es))


(deftest Graph-protocol:Graph

  (testing "adding nodes"
    (is (= (-> (empty-g)
               (loom/add-nodes* [1])
               loom/nodes)
           #{1})))

  (testing "adding an edge"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2]]))]
      (is (= #{[1 2] [2 1]}
             (into #{} (->> (loom/edges g) (map (juxt loom/src loom/dest))))))))
  
  (testing "adding two identical edges"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (and
           (= 4 (count (loom/edges g)))
           (true? (loom/has-edge? g 1 2))))))

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


(deftest Graph-protocol:Digraph

  (testing "adding nodes"
    (is (= (-> (empty-dg)
               (loom/add-nodes* [1])
               loom/nodes)
           #{1})))

  (testing "adding an edge"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2]]))]
      (is (= #{[1 2]}
             (into #{} (->> (loom/edges g) (map (juxt loom/src loom/dest))))))))
  
  (testing "adding two identical edges"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (and
           (= 2 (count (loom/edges g)))
           (true? (loom/has-edge? g 1 2))))))

  (testing "out degree with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= 2 (loom/out-degree g 1)))))

  (testing "successors with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= '(2) (loom/successors g 1)))))

  (testing "out-edges with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= '([1 2] [1 2]) (idedges->vec (loom/out-edges g 1)))))))


(deftest Digraph-protocol:Digraph

  (testing "predecessors* with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= '(1) (loom/predecessors* g 2)))))


  (testing "in-degree* with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= 2 (loom/in-degree g 2)))))

  (testing "in-edges with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= '([1 2] [1 2] [0 2]) (idedges->vec (loom/in-edges g 2))))))

  (testing "transpose with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (=
           (idedges->vec (loom/edges g))
           (idedges->vec (loom/edges (-> g loom/transpose loom/transpose))))))))


(deftest EditableGraph-protocol:Graph

  (testing "remove-nodes* with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= #{0 2}
             (loom/nodes (loom/remove-nodes* g [1]))))))

  (testing "remove-nodes leaving empties in :adj with multidigraph"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= [[2 0] [0 2]]
             (idedges->vec (loom/edges (loom/remove-nodes* g [1])))))))

  (testing "remove-edges"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= [[2 0] [0 2]]
             (idedges->vec (loom/edges (loom/remove-edges* g [[1 2]])))))))

  (testing "remove multiple edge"
    (let [g (-> (empty-g)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= 2
             (count (loom/edges (loom/remove-edges* g [(first (loom.graph/edges g))]))))))))


(deftest EditableGraph-protocol:Digraph

  (testing "remove-nodes* with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= #{0 2}
             (loom/nodes (loom/remove-nodes* g [1]))))))

  (testing "remove-nodes leaving empties in :adj with multidigraph"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= [[0 2]]
             (idedges->vec (loom/edges (loom/remove-nodes* g [1])))))))

  (testing "remove-edges"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2] [0 2]]))]
      (is (= [[0 2]]
             (idedges->vec (loom/edges (loom/remove-edges* g [[1 2]])))))))

  (testing "remove multiple edge"
    (let [g (-> (empty-dg)
                (loom/add-edges* [[1 2] [1 2]]))]
      (is (= 1
             (count (loom/edges (loom/remove-edges* g [(first (loom.graph/edges g))]))))))))


(deftest Building-a-graph:All

  (testing "building a graph & edges are right"
    (let [g (gr/multigraph [1 2] [1 2] [0 2])]
      (is (= (idedges->vec (loom/edges g))
             [[1 2] [1 2] [2 1] [2 1] [2 0] [0 2]]))))

  (testing "building a graph & the nodes are right"
    (let [g (gr/multigraph [1 2] [1 2] [0 2])]
      (is (= (loom/nodes g)
             #{0 1 2}))))

  (testing "building a graph & the attrs are right"
    (let [g (gr/multigraph [1 2] [1 2 {:color "white"}] 3)]
      (is (= #{{:color "white"}}
             (-> g :attrs vals set)))))
;; -- digraph
  (testing "building a digraph & edges are right"
    (let [g (gr/multidigraph [1 2] [1 2] [0 2])]
      (is (= (idedges->vec (loom/edges g))
             [[1 2] [1 2] [0 2]]))))

  (testing "building a digraph & the nodes are right"
    (let [g (gr/multidigraph [1 2] [1 2] [0 2])]
      (is (= (loom/nodes g)
             #{0 1 2}))))

  (testing "building a digraph & the attrs are right"
    (let [g (gr/multidigraph [1 2] [1 2 {:color "white"}] 3)]
      (is (= #{{:color "white"}}
             (-> g :attrs vals set))))))


(deftest AttrGraph-protocol:Graph

  (testing "an attr can be added to a node after build and fetched"
    (let [g (gr/multigraph [1 2] [1 2] [0 2] 4)]
      (is (= "black"
             (attr/attr (attr/add-attr g 4 :color "black") 4 :color)))))

  (testing "an attr can be added to a edge during build and fetched"
    (let [g (gr/multigraph [1 2 {:color "black"}] [1 2] [0 2] 4)]
      (is (= "black"
             (val (first (attr/attr g 1 2 :color)))))))

  (testing "specifying an edge as a vector gives a map of attrs"
    (let [g (gr/multigraph [1 2 {:color "black"}] [1 2 {:color "red"}] [0 2] 4)]
      (is (map? (attr/attr g 1 2 :color)))))

  (testing "specifying an edge as a vector gives a map of attrs of the right order"
    (let [g (gr/multigraph [1 2 {:color "black"}] [1 2 {:color "red"}] [0 2] 4)]
      (is (= 2
             (count (attr/attr g 1 2 :color)))))))


(deftest AttrGraph-protocol:Digraph

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
