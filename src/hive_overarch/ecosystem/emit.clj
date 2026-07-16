(ns hive-overarch.ecosystem.emit
  "Emit Overarch model dirs (model.edn + views.edn) from an ecosystem C4Model.
   Pure string builders; `emit!` is the only effectful entry (writer injected)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-overarch.ecosystem.derive :as derive]))

;; ---- overarch node conversion ----

(defn element->node
  "hive C4Element -> Overarch model node (with :tags)."
  [{:keys [id kind name tech description external tags]}]
  (cond-> {:el kind :id id}
    name              (assoc :name name)
    tech              (assoc :tech tech)
    description       (assoc :desc description)
    external          (assoc :external true)
    (seq tags)        (assoc :tags (set tags))))

(defn relation->node
  "hive C4Relation -> Overarch relation node."
  [{:keys [id kind from to name tech]}]
  (cond-> {:el (or kind :rel) :id id :from from :to to}
    name (assoc :name name)
    tech (assoc :tech tech)))

(def ^:private non-layer-tags #{"open" "closed" "external" "iaddon"})

(defn- layer-of [tags] (first (sort (remove non-layer-tags tags))))

(defn layered-element-nodes
  "Project elements -> Overarch nodes grouped into one :context-boundary per
   layer tag (nested via :ct); elements without a layer tag stay top-level."
  [elements]
  (let [by-layer (group-by (comp layer-of :tags) elements)]
    (concat
     (for [[layer els] (sort-by key (dissoc by-layer nil))]
       {:el   :context-boundary
        :id   (keyword "hive" (str "layer-" layer))
        :name layer
        :ct   (into #{} (map element->node) els)})
     (map element->node (get by-layer nil)))))

(defn edn-set-str
  "Seq of maps -> Overarch EDN file body: a set literal, one map per line,
   ordered by :id for stable diffs."
  [header maps]
  (str header "\n#{"
       (str/join "\n  " (map pr-str (sort-by (comp str :id) maps)))
       "\n  }\n"))

;; ---- IAddon pattern (curated, open-safe: describes the published protocol) ----

(def iaddon-model-nodes
  "Container/component altitude of the IAddon plugin pattern."
  [{:el :system :id :hive.iaddon/host :name "hive-mcp host"
    :desc "MCP server core: protocols + registries + orchestrators only; no concrete backend deps"
    :ct #{{:el :container :id :hive.iaddon/loader :name "addons.manifest loader"
           :desc "Scans classpath for META-INF/hive-addons/*.edn, validates, topo-sorts by :addon/dependencies, resolves :addon/init-fn"}
          {:el :container :id :hive.iaddon/registry :name "addons.core registry"
           :desc "register! -> initialize! -> instance store; shutdown! reclaims"}
          {:el :container :id :hive.iaddon/protocol :name "hive-addon.protocol/IAddon" :tech "defprotocol"
           :desc "addon-id, addon-type, capabilities, initialize!, shutdown!, tools, schema-extensions, health, excluded-tools, hooks"}
          {:el :container :id :hive.iaddon/surface :name "MCP tool surface"
           :desc "Merged tool set; excluded-tools lets a smarter addon shadow a basic one"}}}
   {:el :system :id :hive.iaddon/addon :name "Addon (any)"
    :desc "Standalone lib plugging capabilities into the host"
    :ct #{{:el :container :id :hive.iaddon/manifest :name "META-INF/hive-addons/<id>.edn"
           :desc ":addon/id, :addon/init-ns, :addon/init-fn, :addon/capabilities, :addon/dependencies"}
          {:el :container :id :hive.iaddon/record :name "IAddon record"
           :desc "reify/defrecord implementing the protocol; owns its lifecycle + emitters"}}}])

(def iaddon-rel-nodes
  [{:el :rel :id :hive.iaddon.rel/loader-discovers-manifest
    :from :hive.iaddon/loader :to :hive.iaddon/manifest :name "discovers on classpath"}
   {:el :rel :id :hive.iaddon.rel/loader-constructs-record
    :from :hive.iaddon/loader :to :hive.iaddon/record :name "resolves :addon/init-fn, constructs"}
   {:el :rel :id :hive.iaddon.rel/registry-registers-record
    :from :hive.iaddon/registry :to :hive.iaddon/record :name "register! + initialize!"}
   {:el :rel :id :hive.iaddon.rel/record-implements-protocol
    :from :hive.iaddon/record :to :hive.iaddon/protocol :name "implements"}
   {:el :rel :id :hive.iaddon.rel/record-contributes-tools
    :from :hive.iaddon/record :to :hive.iaddon/surface :name "contributes tools + schema-extensions"}
   {:el :rel :id :hive.iaddon.rel/registry-health
    :from :hive.iaddon/registry :to :hive.iaddon/record :name "health / shutdown!"}])

(def iaddon-views
  [{:el :container-view :id :hive.iaddon/container-view
    :title "IAddon plugin pattern"
    :ct [{:ref :hive.iaddon/host} {:ref :hive.iaddon/addon}
         {:ref :hive.iaddon.rel/loader-discovers-manifest}
         {:ref :hive.iaddon.rel/loader-constructs-record}
         {:ref :hive.iaddon.rel/registry-registers-record}
         {:ref :hive.iaddon.rel/record-implements-protocol}
         {:ref :hive.iaddon.rel/record-contributes-tools}]}
   {:el :dynamic-view :id :hive.iaddon/lifecycle-view
    :title "IAddon lifecycle: discover -> register -> initialize! -> contribute"
    :ct [{:ref :hive.iaddon.rel/loader-discovers-manifest :index 1}
         {:ref :hive.iaddon.rel/loader-constructs-record :index 2}
         {:ref :hive.iaddon.rel/registry-registers-record :index 3}
         {:ref :hive.iaddon.rel/record-implements-protocol :index 4}
         {:ref :hive.iaddon.rel/record-contributes-tools :index 5}
         {:ref :hive.iaddon.rel/registry-health :index 6}]}])

;; ---- ecosystem views ----

(defn ecosystem-views
  "Views over the derived ecosystem model + mind-map concepts."
  [suffix]
  [{:el :system-landscape-view :id (keyword "hive.views" (str "landscape-" suffix))
    :spec {:selection {:namespace "hive"} :include :related}
    :title (str "hive ecosystem landscape (" suffix ")")}
   {:el :system-landscape-view :id (keyword "hive.views" (str "addon-fleet-" suffix))
    :spec {:selection {:namespace "hive" :tag "iaddon"} :include :related}
    :title (str "IAddon fleet (" suffix ")")}
   {:el :concept-view :id (keyword "hive.views" (str "mindmap-" suffix))
    :spec {:selection {:namespace "hive.mind"} :include :relations}
    :title (str "hive ecosystem mind-map (" suffix ")")}])

(defn model-files
  "C4Model -> {\"model.edn\" body, \"views.edn\" body} for one Overarch dir."
  [model suffix]
  (let [mind (derive/mindmap-model model)]
    {"model.edn"
     (edn-set-str ";; generated by hive-overarch.ecosystem.emit — do not hand-edit"
                  (concat (layered-element-nodes (:elements model))
                          (map relation->node (:relations model))
                          (map element->node (:elements mind))
                          (map relation->node (:relations mind))
                          iaddon-model-nodes
                          iaddon-rel-nodes))
     "views.edn"
     (edn-set-str ";; generated by hive-overarch.ecosystem.emit — do not hand-edit"
                  (concat (ecosystem-views suffix) iaddon-views))}))

(defn emit!
  "Write full + open Overarch model dirs under out-root.
   Returns Result<{:written [paths]}>. write-fn defaults to spit."
  ([model out-root] (emit! model out-root {}))
  ([model out-root {:keys [write-fn] :or {write-fn (fn [path body]
                                                     (io/make-parents path)
                                                     (spit path body))}}]
   (r/try-effect* :ecosystem/emit-failed
     (let [dirs {"ecosystem-full" (model-files model "full")
                 "ecosystem-open" (model-files (derive/restrict-open model) "open")}
           written (vec (for [[dir files] dirs
                              [fname body] files
                              :let [path (str out-root "/" dir "/" fname)]]
                          (do (write-fn path body) path)))]
       {:written written}))))
