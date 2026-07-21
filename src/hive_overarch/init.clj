(ns hive-overarch.init
  "IAddon entry point for hive-overarch. Loaded BY hive-mcp via the
   META-INF/hive-addons classpath manifest; resolves the IAddon protocol and
   the command-contribution registry reflectively so production code never
   hard-depends on hive-mcp. Contributes the architecture subcommands
   (arch-snapshot|arch-render|arch-persona|arch-list) into the existing 'code'
   consolidated supertool at initialize! time. The code tool's merged handler
   re-resolves contributions per call, so commands surface live without a
   one-shot boot-time tool registration."
  (:require [clojure.edn :as edn]
            [hive-overarch.derive.ecosystem :as eco-source]
            [hive-overarch.ecosystem.emit :as eco-emit]
            [hive-overarch.store.kg :as store]
            [hive-overarch.orchestrator :as orch]
            [hive-overarch.hive :as hive]
            [hive-overarch.protocols :as p]
            [hive-dsl.result :as r]))

(defn- try-resolve [sym] (r/rescue nil (requiring-resolve sym)))

(defonce ^:private system (delay (orch/make-system)))
(defonce ^:private addon-instance (atom nil))

(defn- result->str [res]
  (if (r/ok? res)
    (let [v (:ok res)] (if (string? v) v (pr-str v)))
    (str "ERROR: " (pr-str res))))

(defn- param-scope [params]
  (or (get params "scope") (get params "path")))

(defn- h-snapshot [params]
  (result->str (orch/snapshot! @system (param-scope params) {})))

(defn- h-render [params]
  (let [kind  (some-> (get params "view") keyword)
        focus (some-> (get params "focus") keyword)
        fmt   (or (some-> (get params "format") keyword) :plantuml)]
    (result->str
      (orch/render! @system (param-scope params)
                    (cond-> {:format fmt}
                      kind  (assoc :view-kind kind)
                      focus (assoc :focus focus))))))

(defn- h-persona [params]
  (let [eid (some-> (or (get params "element") (get params "function")) keyword)]
    (result->str (r/map-ok (orch/persona! @system (param-scope params) eid)
                           :prompt-fragment))))

(defn- h-list [params]
  (result->str (orch/list-snapshots @system (param-scope params))))

(defn- eco-config [params]
  (if-let [inline (get params "config-edn")]
    (r/rescue (r/err :ecosystem/config-invalid {:config-edn inline})
              (r/ok (edn/read-string inline)))
    (if-let [path (get params "config")]
      (eco-source/load-config path)
      (r/ok {}))))

(defn- h-eco-emit [params]
  (result->str
   (r/let-ok [config  (eco-config params)
              model   (p/derive-model (eco-source/ecosystem-source {:config config})
                                      (get params "root") {})
              emitted (eco-emit/emit! model (or (get params "out") "models"))]
     (if (contains? #{"true" "1" true} (get params "persist"))
       (r/map-ok (p/put-snapshot (store/kg-store) model)
                 (fn [ref] (assoc emitted :snapshot (into {} ref))))
       (r/ok emitted)))))

(def ^:private commands
  {"arch-snapshot" {:handler     h-snapshot
                    :params      {"scope" {:type "string"
                                           :description "Project scope/id (or path) to derive the model from"}}
                    :description "Derive a C4 architecture snapshot from carto and persist it"}
   "arch-render"   {:handler     h-render
                    :params      {"scope"  {:type "string" :description "Project scope (or path)"}
                                  "view"   {:type "string"
                                            :description "View kind: system-landscape-view|context-view|container-view|component-view"}
                                  "focus"  {:type "string" :description "Optional element id to focus the view"}
                                  "format" {:type "string" :description "Output format (plantuml)"}}
                    :description "Render the latest architecture snapshot to a diagram string"}
   "arch-persona"  {:handler     h-persona
                    :params      {"scope"   {:type "string" :description "Project scope (or path)"}
                                  "element" {:type "string" :description "C4 element id to build a persona for"}}
                    :description "Project a bounded-context persona prompt from the latest snapshot"}
   "arch-list"     {:handler     h-list
                    :params      {"scope" {:type "string" :description "Project scope (or path)"}}
                    :description "List persisted C4 architecture snapshots for a scope"}
   "arch-eco-emit" {:handler     h-eco-emit
                    :params      {"root"       {:type "string"
                                                :description "Monorepo root to probe (deps.edn graph, git remotes, addon manifests)"}
                                  "out"        {:type "string"
                                                :description "Output dir for Overarch model dirs (default: models)"}
                                  "config"     {:type "string"
                                                :description "Path to an EcoConfig EDN file (orgs, layer table, visibility rules)"}
                                  "config-edn" {:type "string"
                                                :description "Inline EcoConfig EDN (overrides config path)"}
                                  "persist"    {:type "string"
                                                :description "\"true\" to also persist the full model as a c4-snapshot via IModelStore"}}
                    :description "Derive ecosystem C4 model + mind-map from monorepo facts and emit full + open Overarch model dirs"}})

(defn- make-addon []
  (when (try-resolve 'hive-mcp.addons.protocol/IAddon)
    (let [state (atom {:initialized? false})]
      (reify hive-mcp.addons.protocol/IAddon
        (addon-id   [_] "hive.overarch")
        (addon-type [_] :native)
        (capabilities [_] #{:tools})
        (initialize! [_ _config]
          (if (:initialized? @state)
            {:success? true :already-initialized? true}
            (let [res (hive/contribute-commands! "code" :hive.overarch commands)]
              (if (r/ok? res)
                (do (reset! state {:initialized? true})
                    ;; C4 snapshots are stored as "c4-snapshot" memory entries
                    ;; fetched only by tag/id/project-id — declare them
                    ;; non-semantic so the write path skips embedding the
                    ;; (oversized) serialized-model content. Best-effort.
                    (hive/declare-non-semantic-type! "c4-snapshot")
                    {:success? true :errors [] :metadata {:tool "code" :commands (count commands)}})
                {:success? false :errors [res]}))))
        (shutdown! [_]
          (hive/retract-commands! :hive.overarch)
          (reset! state {:initialized? false})
          nil)
        (tools [_] [])
        (schema-extensions [_] {})
        (health [_]
          (if (:initialized? @state)
            {:status :ok   :details {:commands (count commands)}}
            {:status :down :details {:reason "not initialized"}}))
        (excluded-tools [_] #{})
        (hooks [_] {})))))

(defn init-as-addon!
  "Zero-arg loader entry point. Constructs the IAddon, self-registers it, runs
   the registration sweep, and contributes the arch-* subcommands into the
   'code' supertool. Returns {:registered [cmd...] :total N} reflecting the
   commands actually contributed (0 when hive-mcp is absent)."
  []
  (if-let [addon (make-addon)]
    (let [register! (try-resolve 'hive-mcp.addons.core/register-addon!)
          init!     (try-resolve 'hive-mcp.addons.core/init-addon!)
          contributed? (boolean
                        (when (and register! init!)
                          (register! addon)
                          (init! "hive.overarch" {})
                          (reset! addon-instance addon)
                          true))
          cmds (vec (sort (keys commands)))]
      (if contributed?
        {:registered cmds :total (count cmds) :tool "code"}
        {:registered [] :total 0}))
    {:registered [] :total 0}))

(defn addon-ctor
  "Pure constructor - (config -> IAddon | nil). Reifies the hive.overarch IAddon
   WITHOUT the register!/init! self-registration that `init-as-addon!` performs.
   Returns nil when the hive-mcp IAddon protocol is absent from the classpath
   (graceful). The mounter (hive-addon.mount) resolves this via :addon/init-fn
   and itself drives register!/initialize!. Additive: the self-registering
   `init-as-addon!` path remains for the current hive-mcp loader."
  [_config]
  (make-addon))