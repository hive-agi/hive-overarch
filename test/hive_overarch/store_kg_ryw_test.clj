(ns hive-overarch.store-kg-ryw-test
  "Read-your-writes for KgModelStore: the hive memory write path is async, so
   a snapshot! followed at once by render! must still find the model. Tested
   through the store's memory PORT with a lagging stub (writes never become
   visible to queries), plus a trifecta over the pure ref merge."
  (:refer-clojure :exclude [ref])
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-overarch.model :as m]
            [hive-overarch.protocols :as p]
            [hive-overarch.store.kg :as kg]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn- lagging-memory
  "Memory port whose writes succeed but are never visible to query (the
   async write has not landed yet)."
  []
  {:add!  (fn [_entry] (r/ok "queued-id"))
   :query (fn [_opts] (r/ok []))})

(defn- snapshot [sid scope inst]
  (m/model [(m/element :c4/a :system "a")] []
           (m/map->Provenance {:snapshot-id sid :scope scope :taken-at inst})))

(deftest read-your-writes-despite-async-memory
  (let [store (kg/kg-store (lagging-memory))
        s1    (snapshot "snap-1" "demo" #inst "2026-01-01")
        s2    (snapshot "snap-2" "demo" #inst "2026-06-01")
        other (snapshot "snap-x" "elsewhere" #inst "2026-09-01")]
    (testing "before any write, latest is :store/no-snapshots"
      (is (= :store/no-snapshots (:error (p/latest store "demo")))))
    (is (r/ok? (p/put-snapshot store s1)))
    (is (r/ok? (p/put-snapshot store s2)))
    (is (r/ok? (p/put-snapshot store other)))
    (testing "latest right after put sees the just-written snapshot"
      (is (= s2 (:ok (p/latest store "demo")))))
    (testing "get-snapshot by id is served locally"
      (is (= s1 (:ok (p/get-snapshot store "snap-1")))))
    (testing "list-snapshots is scope-filtered"
      (is (= ["snap-1" "snap-2"] (mapv :id (:ok (p/list-snapshots store "demo"))))))))

(deftest failed-write-is-not-cached
  (let [store (kg/kg-store {:add!  (fn [_] (r/err :memory/add-failed {}))
                            :query (fn [_] (r/ok []))})]
    (is (r/err? (p/put-snapshot store (snapshot "snap-1" "demo" #inst "2026-01-01"))))
    (is (= :store/no-snapshots (:error (p/latest store "demo"))))))

(deftest cache-evicts-once-persisted
  (let [entries (atom [])
        memory  {:add!  (fn [e] (swap! entries conj e) (r/ok "id"))
                 :query (fn [{:keys [project-id tags]}]
                          (r/ok (filter #(or (= project-id (:project-id %))
                                             (some (set tags) (:tags %)))
                                        @entries)))}
        store   (kg/kg-store memory)
        s1      (snapshot "snap-1" "demo" #inst "2026-01-01")]
    (is (r/ok? (p/put-snapshot store s1)))
    (is (contains? @(:recent store) "snap-1"))
    (testing "a list that sees the persisted entry evicts it from the cache"
      (is (= ["snap-1"] (mapv :id (:ok (p/list-snapshots store "demo")))))
      (is (empty? @(:recent store))))
    (testing "get-snapshot then falls through to memory"
      (is (= "snap-1" (get-in (:ok (p/get-snapshot store "snap-1"))
                              [:provenance :snapshot-id]))))))

;; ---- trifecta over the pure merge ----

(defn- ref [id] (m/->SnapshotRef id "demo" nil 1))

(def ^:private gen-ids (gen/vector (gen/elements ["a" "b" "c" "d" "e"]) 0 4))

(def ^:private gen-input
  (gen/fmap (fn [[p q]] [(mapv ref (distinct p)) (mapv ref (distinct q))])
            (gen/tuple gen-ids gen-ids)))

(defn merged-ids
  "Subject: the ids merge-refs yields, in order."
  [input]
  (mapv :id (kg/merge-refs input)))

(defn- valid-merge?
  "Ids form a vector with no duplicates."
  [ids]
  (and (vector? ids) (= (count ids) (count (distinct ids)))))

(defn- mut-drop-recent [[persisted _recent]]
  (mapv :id persisted))

(defn- mut-no-dedupe [[persisted recent]]
  (mapv :id (concat persisted recent)))

(defn- mut-recent-first [[persisted recent]]
  (vec (distinct (concat (map :id recent) (map :id persisted)))))

(deftrifecta merge-refs-trifecta
  hive-overarch.store-kg-ryw-test/merged-ids
  {:golden-path "test/golden/hive-overarch/trifecta-merge-refs.edn"
   :cases       {:both-empty       [[] []]
                 :persisted-only   [[(ref "a") (ref "b")] []]
                 :recent-only      [[] [(ref "a")]]
                 :disjoint         [[(ref "a")] [(ref "b")]]
                 :overlap          [[(ref "a") (ref "b")] [(ref "b") (ref "c")]]
                 :recent-precedes  [[(ref "b")] [(ref "a") (ref "b")]]}
   :gen         gen-input
   :pred        valid-merge?
   :num-tests   100
   :mutations   [["drop-recent"   mut-drop-recent]
                 ["no-dedupe"     mut-no-dedupe]
                 ["recent-first"  mut-recent-first]]})
