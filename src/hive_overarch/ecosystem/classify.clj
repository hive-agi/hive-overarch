(ns hive-overarch.ecosystem.classify
  "Ordered rule-chains classifying ProjectFacts (visibility, layer). Pure
   mechanism: the particular monorepo arrives as an EcoConfig map; a new case
   is a new config entry or rule — the fold never changes (OCP). With an empty
   config everything classifies :closed/:lib (leak-safe)."
  (:require [malli.core :as mc]
            [hive-overarch.ecosystem.schema :as s]))

(defprotocol IEcoRule
  "One classification rule over a ProjectFact."
  (-hit?    [this fact] "true when this rule decides the fact")
  (-verdict [this fact] "the decision for a hit fact"))

(defrecord Rule [id hit? verdict]
  IEcoRule
  (-hit?    [_ f] (boolean (hit? f)))
  (-verdict [_ f] (if (fn? verdict) (verdict f) verdict)))

(defn rule
  "Build a named rule. verdict is a value or (fn [fact] ...)."
  [id hit? verdict]
  (->Rule id hit? verdict))

(defn classify
  "Fold fact through ordered rules; first hit wins, else fallback."
  [rules fallback fact]
  (or (some #(when (-hit? % fact) (-verdict % fact)) rules) fallback))

;; ---- visibility ----

(defn- remote-matches? [patterns fact]
  (let [remote (str (:remote fact))]
    (boolean (some #(re-find (re-pattern %) remote) patterns))))

(defn visibility-rules
  "EcoConfig -> ordered visibility rules: overrides, then open-remote
   patterns, then external-remote patterns."
  [{:keys [visibility-overrides open-remote-patterns external-remote-patterns]}]
  [(rule :visibility/override
         #(contains? visibility-overrides (:name %))
         #(get visibility-overrides (:name %)))
   (rule :remote/open
         #(remote-matches? open-remote-patterns %)
         :open)
   (rule :remote/external
         #(remote-matches? external-remote-patterns %)
         :external)])

(defn visibility
  "EcoConfig + ProjectFact -> Visibility. Unmatched remotes are :closed."
  [config fact]
  (classify (visibility-rules config) :closed fact))

(mc/=> visibility [:=> [:cat s/EcoConfig s/ProjectFact] s/Visibility])

;; ---- layer ----

(defn layer-rules
  "EcoConfig -> ordered layer rules (the :layer-table lookup)."
  [{:keys [layer-table]}]
  [(rule :layer/table
         #(contains? layer-table (:name %))
         #(get layer-table (:name %)))])

(defn layer
  "EcoConfig + ProjectFact -> Layer, :lib when no rule hits."
  [config fact]
  (classify (layer-rules config) :lib fact))

(mc/=> layer [:=> [:cat s/EcoConfig s/ProjectFact] s/Layer])

(defn classify-project
  "EcoConfig + ProjectFact -> ClassifiedProject."
  [config fact]
  (assoc fact
         :visibility (visibility config fact)
         :layer      (layer config fact)))

(mc/=> classify-project [:=> [:cat s/EcoConfig s/ProjectFact] s/ClassifiedProject])
