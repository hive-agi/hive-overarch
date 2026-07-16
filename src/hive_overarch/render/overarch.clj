(ns hive-overarch.render.overarch
  "IRenderer adapter over the upstream Overarch library. Converts a hive C4View
   into an Overarch input model, builds the relational model, and renders a
   PlantUML diagram string in-memory (no files)."
  (:require [clojure.string :as str]
            [org.soulspace.overarch.domain.model :as oa-model]
            [org.soulspace.overarch.adapter.reader.input-model-reader :as oa-imr]
            [org.soulspace.overarch.application.render]
            [org.soulspace.overarch.adapter.render.plantuml :as oa-puml]
            ;; side-effecting: register render-plantuml-view methods (:c4-view, :uml-view)
            [org.soulspace.overarch.adapter.render.plantuml.c4]
            [org.soulspace.overarch.adapter.render.plantuml.uml]
            [hive-overarch.protocols :as p]
            [hive-dsl.result :as r]))

(defn- element->node [{:keys [id kind name tech description external]}]
  (cond-> {:el kind :id id}
    name        (assoc :name name)
    tech        (assoc :tech tech)
    description (assoc :desc description)
    external    (assoc :external true)))

(defn- relation->rel [{:keys [id kind from to name tech]}]
  (cond-> {:el (or kind :rel) :id id :from from :to to}
    name (assoc :name name)
    tech (assoc :tech tech)))

(defn- view-id [{:keys [kind focus]}]
  (keyword "c4-view" (str (name kind)
                          (when focus (str "-" (name focus))))))

(defn- view->map [{:keys [kind title elements relations] :as view}]
  {:el    kind
   :id    (view-id view)
   :title (or title (name kind))
   :ct    (vec (concat (map (fn [e] {:ref (:id e)}) elements)
                       (map (fn [rel] {:ref (:id rel)}) relations)))})

(defn c4view->input
  "C4View -> an Overarch input model SET (#{nodes... rels... view})."
  [{:keys [elements relations] :as view}]
  (set (concat (map element->node elements)
               (map relation->rel relations)
               [(view->map view)])))

(defn render-plantuml
  "C4View -> Result<plantuml-string>."
  [view]
  (r/try-effect* :render/plantuml-failed
    (let [oaview (view->map view)
          input  (c4view->input view)
          built  (oa-model/traverse oa-imr/->relational-model input)
          lines  (oa-puml/render-plantuml-view built {} oaview)]
      (str/join "\n" (flatten lines)))))

(defrecord OverarchRenderer []
  p/IRenderer
  (render-view [_ view fmt]
    (case fmt
      (:plantuml :puml) (render-plantuml view)
      (r/err :render/unsupported-format {:format fmt :supported #{:plantuml}}))))

(defn renderer [] (->OverarchRenderer))
