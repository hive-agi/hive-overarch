(ns hive-overarch.orchestrator
  "Composition root + pipeline. Wires the default record implementations behind
   the protocols and exposes the four addon operations. Depends only on the
   protocols (DIP); concrete adapters are injected here."
  (:require [hive-overarch.protocols :as p]
            [hive-overarch.derive.carto :as carto]
            [hive-overarch.derive.cluster :as cluster]
            [hive-overarch.store.kg :as store]
            [hive-overarch.view :as view]
            [hive-overarch.render.overarch :as render]
            [hive-overarch.persona :as persona]
            [hive-dsl.result :as r]))

(defrecord OverarchSystem [source store projection renderer persona])

(defn make-system
  ([] (make-system {}))
  ([{:keys [clusterer-type] :or {clusterer-type :greedy-cohesion}}]
   (->OverarchSystem (carto/carto-source (cluster/clusterer clusterer-type))
             (store/kg-store)
             (view/default-projection)
             (render/renderer)
             (persona/projector))))

(defn snapshot!
  "Derive a fresh C4 model from carto and persist it. -> Result<SnapshotRef>."
  [{:keys [source store]} scope opts]
  (r/let-ok [model (p/derive-model source scope opts)]
    (p/put-snapshot store model)))

(defn render!
  "Render the latest snapshot for scope to a diagram. -> Result<String>."
  [{:keys [store projection renderer]} scope
   {:keys [view-kind focus format]
    :or   {view-kind :system-landscape-view format :plantuml}}]
  (r/let-ok [model (p/latest store scope)
             view  (p/project projection model view-kind focus)]
    (p/render-view renderer view format)))

(defn persona!
  "Project a bounded-context persona from the latest snapshot. -> Result<PersonaContext>."
  [{:keys [store persona]} scope element-id]
  (r/let-ok [model (p/latest store scope)]
    (p/project-persona persona model element-id)))

(defn list-snapshots
  "List persisted snapshot refs for scope. -> Result<seq<SnapshotRef>>."
  [{:keys [store]} scope]
  (p/list-snapshots store scope))
