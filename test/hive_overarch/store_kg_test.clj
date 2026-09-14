(ns hive-overarch.store-kg-test
  "Tests the KgModelStore snapshot round-trip against a fake (atom-backed)
   memory, without requiring the hive stack."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-overarch.hive :as hive]
            [hive-overarch.model :as m]
            [hive-overarch.protocols :as p]
            [hive-overarch.store.kg :as kg]))

(defn- make-fake-store
  "Build a fake in-memory store: an atom holding entry maps.
   Returns {:add! fn :query fn} matching the shape expected by
   with-redefs of hive/memory-add! and hive/memory-query."
  []
  (let [entries (atom [])
        counter (atom 0)]
    {:add! (fn [entry]
             (let [id (str "id-" (swap! counter inc))]
               (swap! entries conj (assoc entry :id id))
               (r/ok id)))
     :query (fn [opts]
              (let [matches (filter
                             (fn [e]
                               (and (or (not (:type opts))
                                        (= (:type opts) (:type e)))
                                    (or (not (:tags opts))
                                        (every? (set (:tags e)) (:tags opts)))
                                    (or (not (:project-id opts))
                                        (= (:project-id opts) (:project-id e)))))
                             @entries)
                    limit (:limit opts 100)]
                (r/ok (take limit matches))))}))

(deftest snapshot-round-trip
  (let [fake   (make-fake-store)
        store  (kg/->KgModelStore)
        el-a   (m/element :c4/a :system "a" :tags #{"t"} :carto-refs ["x.y/z"])
        el-b   (m/element :c4/b :system "b")
        rel    (m/relation :c4-rel/r0 :c4/a :c4/b :name "connects")
        prov1  (m/map->Provenance
                {:snapshot-id "snap-1" :scope "demo"
                 :taken-at #inst "2026-01-01"})
        prov2  (m/map->Provenance
                {:snapshot-id "snap-2" :scope "demo"
                 :taken-at #inst "2026-06-01"})
        model1 (m/model [el-a el-b] [rel] prov1)
        model2 (m/model [el-a el-b] [rel] prov2)]
    (with-redefs [hive/memory-add! (:add! fake)
                  hive/memory-query (:query fake)]

      (testing "put-snapshot returns ok Result for two snapshots"
        (let [r1 (p/put-snapshot store model1)]
          (is (r/ok? r1))
          (is (= "snap-1" (:id (:ok r1)))))
        (let [r2 (p/put-snapshot store model2)]
          (is (r/ok? r2))
          (is (= "snap-2" (:id (:ok r2))))))

      (testing "get-snapshot of first id returns model equal to the one put in"
        (let [result (p/get-snapshot store "snap-1")]
          (is (r/ok? result))
          (is (= model1 (:ok result)))))

      (testing "carto-refs survive the round-trip"
        (let [result (p/get-snapshot store "snap-1")
              elem   (first (:elements (:ok result)))]
          (is (= #{"x.y/z"} (:carto-refs elem)))))

      (testing "latest for scope demo returns the snapshot with newest taken-at"
        (let [result (p/latest store "demo")]
          (is (r/ok? result))
          (is (= model2 (:ok result)))))

      (testing "get-snapshot of unknown id returns err Result"
        (let [result (p/get-snapshot store "non-existent")]
          (is (r/err? result))
          (is (= :store/snapshot-not-found (:error result))))))))
