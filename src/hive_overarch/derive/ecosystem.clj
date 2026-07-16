(ns hive-overarch.derive.ecosystem
  "IModelSource that derives a C4 model SNAPSHOT from monorepo facts:
   deps.edn sibling coordinates -> relations, git remotes -> visibility tags,
   IAddon manifests -> addon tags + plugs-into relations. Millisecond-cheap
   and reproducible — take a new snapshot instead of updating an old one.
   The particular monorepo (orgs, layer table, host) is an EcoConfig value."
  (:require [clojure.edn :as edn]
            [hive-dsl.result :as r]
            [malli.core :as mc]
            [hive-overarch.ecosystem.derive :as eco]
            [hive-overarch.ecosystem.emit :as emit]
            [hive-overarch.ecosystem.probe :as probe]
            [hive-overarch.ecosystem.schema :as s]
            [hive-overarch.model :as m]
            [hive-overarch.protocols :as p]))

(defn load-config
  "Read + validate an EcoConfig from an EDN file path (via the probe fs seam).
   Returns Result<EcoConfig>."
  ([path] (load-config path {}))
  ([path {:keys [fs] :or {fs probe/default-fs}}]
   (let [text   ((:read-file fs) (str path))
         config (when text (r/rescue nil (edn/read-string text)))]
     (cond
       (nil? text)
       (r/err :ecosystem/config-not-found {:path (str path)})

       (mc/validate s/EcoConfig config)
       (r/ok config)

       :else
       (r/err :ecosystem/config-invalid
              {:path    (str path)
               :explain (mc/explain s/EcoConfig config)})))))

(defrecord EcosystemSource [fs config]
  p/IModelSource
  (derive-model [_ scope opts]
    (let [cfg (or (:config opts) config {})]
      (r/let-ok [facts (probe/collect-facts scope (cond-> {:config cfg}
                                                    fs (assoc :fs fs)))]
        (r/ok (eco/facts->model
               facts
               {:config cfg
                :provenance (m/->Provenance (str "eco-" (System/currentTimeMillis))
                                            (str scope) (str (java.time.Instant/now))
                                            nil :ecosystem nil)}))))))

(defn ecosystem-source
  "Build an EcosystemSource. opts: {:fs probe-seam, :config EcoConfig}."
  ([] (ecosystem-source {}))
  ([{:keys [fs config]}] (->EcosystemSource fs config)))

(defn emit-from-config!
  "Probe root, derive with the EcoConfig at config-path (empty config when nil),
   emit full + open Overarch model dirs under out (default \"models\").
   -> Result<{:written [paths]}>. fs/write-fn are injectable seams."
  [{:keys [root out config-path fs write-fn]}]
  (r/let-ok [config (if config-path
                      (load-config config-path (cond-> {} fs (assoc :fs fs)))
                      (r/ok {}))
             model  (p/derive-model (ecosystem-source {:fs fs :config config}) root {})]
    (emit/emit! model (or out "models") (cond-> {} write-fn (assoc :write-fn write-fn)))))
