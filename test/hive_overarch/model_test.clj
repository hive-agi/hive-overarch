(ns hive-overarch.model-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-overarch.model :as m]))

(defn- sample-model []
  (let [a   (m/element :c4/auth :system "Auth"  :tech "Clojure" :carto-refs ["auth.core/login"])
        b   (m/element :c4/db   :system "DB"    :tech "Datalevin")
        c   (m/element :c4/web  :system "Web"   :tech "Clojure")
        r1  (m/relation :c4-rel/r0 :c4/web  :c4/auth :name "calls")
        r2  (m/relation :c4-rel/r1 :c4/auth :c4/db  :name "reads")
        prov (m/->Provenance "c4-1" "demo" "2026-06-22T00:00:00Z" nil :carto :greedy-cohesion)]
    (m/model [a b c] [r1 r2] prov)))

(deftest index-and-relations
  (let [model (sample-model)]
    (testing "index-by-id"
      (is (= "Auth" (:name ((m/index-by-id model) :c4/auth)))))
    (testing "out/in relations"
      (is (= [:c4/db]  (map :to   (m/out-relations model :c4/auth))))
      (is (= [:c4/web] (map :from (m/in-relations  model :c4/auth)))))
    (testing "neighbor-ids"
      (is (= #{:c4/web :c4/db} (m/neighbor-ids model :c4/auth))))))

(deftest subgraph-is-focused
  (let [model (sample-model)
        {:keys [focus elements relations]} (m/subgraph model :c4/auth)]
    (is (= :c4/auth (:id focus)))
    (is (= #{:c4/auth :c4/web :c4/db} (set (map :id elements))))
    (is (= 2 (count relations)))))
