(ns hive-overarch.store.kg
  "IModelStore that persists immutable C4 snapshots as hive memory entries
   (type \"c4-snapshot\"). Snapshots are values: serialized to plain EDN data
   (no records) so they round-trip via clojure.edn without custom readers.

   Read-your-writes: the memory write path is asynchronous, so a query issued
   right after put-snapshot may not see the new entry yet. The store keeps the
   snapshots IT wrote in a local `recent` atom and folds them into every read,
   so snapshot! followed at once by render!/persona! always finds the model."
  (:require [clojure.edn :as edn]
            [hive-overarch.protocols :as p]
            [hive-overarch.hive :as hive]
            [hive-overarch.model :as m]
            [hive-dsl.result :as r]))

(defn- model->data [model]
  {:elements   (mapv #(into {} %) (:elements model))
   :relations  (mapv #(into {} %) (:relations model))
   :provenance (into {} (:provenance model))})

(defn- data->model [d]
  (m/->C4Model (mapv m/map->C4Element (:elements d))
               (mapv m/map->C4Relation (:relations d))
               (m/map->Provenance (:provenance d))))

(defn- entry->ref [e]
  (let [prov (:provenance (edn/read-string (:content e)))]
    (m/->SnapshotRef (:snapshot-id prov) (:scope prov) (:taken-at prov)
                     (count (:elements (edn/read-string (:content e)))))))

(defn- model->ref [model]
  (let [prov (:provenance model)]
    (m/->SnapshotRef (:snapshot-id prov) (:scope prov) (:taken-at prov)
                     (count (:elements model)))))

(defn merge-refs
  "Pure. [persisted recent] -> vector of SnapshotRefs: every persisted ref,
   plus each recent (locally written) ref whose id the persisted set does not
   carry yet. Persisted refs win on an id collision; order is persisted first,
   then recent in the order given."
  [[persisted recent]]
  (let [seen (into #{} (map :id) persisted)]
    (into (vec persisted) (remove #(contains? seen (:id %))) recent)))

(def ^:private default-memory
  "Memory port over the hive bridge. Calls go through the vars at call time."
  {:add!  (fn [entry] (hive/memory-add! entry))
   :query (fn [opts] (hive/memory-query opts))})

(defrecord KgModelStore [recent memory]
  p/IModelStore
  (put-snapshot [_ model]
    (let [prov  (:provenance model)
          sid   (:snapshot-id prov)
          scope (:scope prov)
          entry {:type       "c4-snapshot"
                 :content    (pr-str (model->data model))
                 :tags       ["c4-snapshot" "overarch" sid (str "scope:" scope)]
                 :project-id scope}]
      (r/let-ok [_id ((:add! (or memory default-memory)) entry)]
        (when recent (swap! recent assoc sid model))
        (r/ok (model->ref model)))))

  (get-snapshot [_ id]
    (if-let [local (some-> recent deref (get id))]
      (r/ok local)
      (r/let-ok [entries ((:query (or memory default-memory))
                          {:type "c4-snapshot" :tags [id] :limit 1})]
        (if-let [e (first entries)]
          (r/ok (data->model (edn/read-string (:content e))))
          (r/err :store/snapshot-not-found {:id id})))))

  (list-snapshots [_ scope]
    (r/let-ok [entries ((:query (or memory default-memory))
                        {:type "c4-snapshot" :project-id scope :limit 100})]
      (let [local (->> (some-> recent deref vals)
                       (filter #(= scope (get-in % [:provenance :scope])))
                       (sort-by #(get-in % [:provenance :taken-at]))
                       (map model->ref))]
        (r/ok (merge-refs [(mapv entry->ref entries) local])))))

  (latest [this scope]
    (r/let-ok [refs (p/list-snapshots this scope)]
      (if-let [newest (last (sort-by :taken-at refs))]
        (p/get-snapshot this (:id newest))
        (r/err :store/no-snapshots {:scope scope})))))

(defn kg-store
  "KgModelStore with a fresh read-your-writes cache. `memory` is an optional
   port map {:add! (entry -> Result<id>) :query (opts -> Result<seq<entry>>)};
   it defaults to the hive memory bridge."
  ([] (kg-store nil))
  ([memory] (->KgModelStore (atom {}) memory)))
