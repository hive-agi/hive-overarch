(ns hive-overarch.store.kg
  "IModelStore that persists immutable C4 snapshots as hive memory entries
   (type \"c4-snapshot\"). Snapshots are values: serialized to plain EDN data
   (no records) so they round-trip via clojure.edn without custom readers."
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

(defrecord KgModelStore []
  p/IModelStore
  (put-snapshot [_ model]
    (let [prov  (:provenance model)
          sid   (:snapshot-id prov)
          scope (:scope prov)
          entry {:type       "c4-snapshot"
                 :content    (pr-str (model->data model))
                 :tags       ["c4-snapshot" "overarch" sid (str "scope:" scope)]
                 :project-id scope}]
      (r/let-ok [_id (hive/memory-add! entry)]
        (r/ok (m/->SnapshotRef sid scope (:taken-at prov) (count (:elements model)))))))

  (get-snapshot [_ id]
    (r/let-ok [entries (hive/memory-query {:type "c4-snapshot" :tags [id] :limit 1})]
      (if-let [e (first entries)]
        (r/ok (data->model (edn/read-string (:content e))))
        (r/err :store/snapshot-not-found {:id id}))))

  (list-snapshots [_ scope]
    (r/let-ok [entries (hive/memory-query {:type "c4-snapshot" :project-id scope :limit 100})]
      (r/ok (mapv entry->ref entries))))

  (latest [this scope]
    (r/let-ok [refs (p/list-snapshots this scope)]
      (if-let [newest (last (sort-by :taken-at refs))]
        (p/get-snapshot this (:id newest))
        (r/err :store/no-snapshots {:scope scope})))))

(defn kg-store [] (->KgModelStore))
