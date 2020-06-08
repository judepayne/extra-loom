(ns extra-loom.alg-generic-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
            [loom.graph :as loom.graph]
            [extra-loom.multigraph :as gr]
            [loom.alg-generic :as loom.alg-generic]))


(def edges [[1 2] [1 5] [2 3] [2 4] [3 6] [4 6] [4 8] [5 6] [7 2]])


(def di (apply loom.graph/digraph edges))


(def multdi0 (apply gr/multidigraph edges))


(def multdi1 (apply gr/multidigraph (conj edges [2 4] [3 6])))  ;; two multiple edges


(deftest pre-edge-traverse1
  (testing "multi-digraph is same as digraph for pre-edge-traverse."
    (is (= (into #{} (loom.alg-generic/pre-edge-traverse #(loom.graph/successors* di %) 1))
           (into #{} (loom.alg-generic/pre-edge-traverse #(loom.graph/successors* multdi0 %) 1))))))

(deftest bf-path1
  (testing "multi-digraph is same as digraph for bf-path."
    (is (= (loom.alg-generic/bf-path #(loom.graph/successors* di %) 1 6)
           (loom.alg-generic/bf-path #(loom.graph/successors* multdi0 %) 1 6)))))


(deftest bf-path2
  (testing "multiple edges make no difference to bf-path."
    (is (= (loom.alg-generic/bf-path #(loom.graph/successors* multdi0 %) 1 6)
           (loom.alg-generic/bf-path #(loom.graph/successors* multdi1 %) 1 6)))))


#?(:cljs (cljs.test/run-tests))
