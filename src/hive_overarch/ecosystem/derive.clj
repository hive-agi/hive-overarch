(ns hive-overarch.ecosystem.derive
  "Promote/Pipeline layer: EcosystemFacts -> C4Model, plus pure projections
   (visibility restriction, mind-map concept model). The particular monorepo
   arrives as an EcoConfig; the derivation itself is generic."
  (:require [malli.core :as mc]
            [hive-overarch.ecosystem.classify :as classify]
            [hive-overarch.ecosystem.schema :as s]
            [hive-overarch.model :as m]))

(defn- project-id [pname] (keyword "hive" pname))

(defn- project-element [{:keys [name layer visibility addon? addon-manifest]}]
  (m/element (project-id name) :system name
             :tech "Clojure"
             :description (or (:addon/description addon-manifest)
                              (str (clojure.core/name layer) " project"))
             :external (= visibility :external)
             :tags (cond-> #{(clojure.core/name visibility)
                             (clojure.core/name layer)}
                     addon? (conj "iaddon"))))

(defn- dep-relations [names {:keys [name deps]}]
  (for [d (sort deps) :when (contains? names d)]
    (m/relation (keyword "hive.rel" (str name "-uses-" d))
                (project-id name) (project-id d)
                :name "depends on")))

(defn- addon-relation [host names {:keys [name addon?]}]
  (when (and host addon? (contains? names host) (not= name host))
    (m/relation (keyword "hive.rel" (str name "-plugs-into-" host))
                (project-id name) (project-id host)
                :name "plugs into" :tech "IAddon")))

(defn facts->model
  "EcosystemFacts + EcoConfig -> C4Model. Every project becomes a :system
   element tagged with its visibility + layer (+ \"iaddon\" when it ships an
   addon manifest); sibling deps become relations; addons additionally plug
   into the config's :host-project."
  ([facts] (facts->model facts {}))
  ([{:keys [root projects]} {:keys [config provenance]}]
   (let [host       (:host-project config)
         classified (mapv #(classify/classify-project (or config {}) %) projects)
         names      (into #{} (map :name) classified)
         elements   (mapv project-element classified)
         relations  (vec (concat (mapcat #(dep-relations names %) classified)
                                 (keep #(addon-relation host names %) classified)))]
     (m/model elements relations
              (or provenance
                  (m/->Provenance nil (str root) nil nil :ecosystem nil))))))

(defn restrict-open
  "C4Model -> C4Model containing ONLY \"open\"-tagged elements and the
   relations among them. The open artifact must not mention closed projects."
  [{:keys [elements relations provenance]}]
  (let [open-els (filterv #(contains? (:tags %) "open") elements)
        keep-ids (into #{} (map :id) open-els)]
    (m/model open-els
             (filterv #(and (keep-ids (:from %)) (keep-ids (:to %))) relations)
             provenance)))

(defn mindmap-model
  "C4Model -> concept-map {:elements :relations}: an ecosystem root concept,
   one concept per layer tag, one concept per project, :has edges root->layer
   and layer->project."
  [{:keys [elements]}]
  (let [layer-tags (into (sorted-set)
                         (mapcat #(remove #{"open" "closed" "external" "iaddon"} (:tags %)))
                         elements)
        root       (m/element :hive.mind/ecosystem :concept "hive ecosystem"
                              :description "All hive projects, grouped by stratum")
        layer-el   (fn [t] (m/element (keyword "hive.mind" (str "layer-" t)) :concept t))
        proj-el    (fn [{:keys [id name tags description]}]
                     (m/element (keyword "hive.mind" (clojure.core/name id)) :concept name
                                :description description :tags tags))
        layer-of   (fn [{:keys [tags]}]
                     (some layer-tags tags))
        rels       (concat
                    (for [t layer-tags]
                      (m/relation (keyword "hive.mind.rel" (str "ecosystem-has-" t))
                                  :hive.mind/ecosystem (keyword "hive.mind" (str "layer-" t))
                                  :kind :has :name "has"))
                    (for [e elements :let [t (layer-of e)] :when t]
                      (m/relation (keyword "hive.mind.rel" (str t "-has-" (clojure.core/name (:id e))))
                                  (keyword "hive.mind" (str "layer-" t))
                                  (keyword "hive.mind" (clojure.core/name (:id e)))
                                  :kind :has :name "has")))]
    {:elements  (vec (concat [root] (map layer-el layer-tags) (map proj-el elements)))
     :relations (vec rels)}))

(mc/=> facts->model [:function
                     [:=> [:cat s/EcosystemFacts] :any]
                     [:=> [:cat s/EcosystemFacts :map] :any]])
