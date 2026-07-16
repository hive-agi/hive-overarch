(ns hive-overarch.persona
  "IPersonaProjector: project the subgraph around a C4 element into a spawn-ready
   PersonaContext. The prompt-fragment is PURE and always produced; ctx-refs are
   a best-effort enrichment minted into the ephemeral context-store."
  (:require [clojure.string :as str]
            [hive-overarch.protocols :as p]
            [hive-overarch.hive :as hive]
            [hive-overarch.model :as m]
            [hive-dsl.result :as r]))

(defn- names-of [idx ids]
  (->> ids (keep idx) (map :name) (remove nil?)))

(defn describe
  "Pure: build the persona prompt-fragment for element-id from its subgraph."
  [model element-id]
  (let [idx   (m/index-by-id model)
        focus (idx element-id)
        scope (get-in model [:provenance :scope])
        outs  (names-of idx (map :to (m/out-relations model element-id)))
        ins   (names-of idx (map :from (m/in-relations model element-id)))]
    (->> [(str "You are the `" (:name focus) "` " (name (:kind focus))
               " in the `" scope "` architecture.")
          (when (:description focus) (str "Responsibility: " (:description focus) "."))
          (when (:tech focus) (str "Tech: " (:tech focus) "."))
          ""
          "You own these namespaces/forms:"
          (str/join "\n" (map #(str "  - " %) (sort (:carto-refs focus))))
          ""
          (when (seq outs) (str "You depend on: " (str/join ", " outs) "."))
          (when (seq ins)  (str "You are depended on by: " (str/join ", " ins) "."))
          ""
          "Stay within your boundary — do not edit code outside your owned namespaces without explicit coordination."]
         (remove nil?)
         (str/join "\n"))))

(defrecord SubgraphPersonaProjector []
  p/IPersonaProjector
  (project-persona [_ model element-id]
    (if-not ((m/index-by-id model) element-id)
      (r/err :persona/unknown-element {:id element-id})
      (let [fragment (describe model element-id)
            scope    (get-in model [:provenance :scope])
            sub      (m/subgraph model element-id)
            ctx      (hive/context-put! (pr-str sub) #{"c4-subgraph"})
            ctx-refs (if (r/ok? ctx) {:architecture (:ok ctx)} {})]
        (r/ok (m/->PersonaContext element-id fragment ctx-refs [] scope))))))

(defn projector [] (->SubgraphPersonaProjector))
