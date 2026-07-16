(ns hive-overarch.ecosystem.schema
  "Malli value objects for ecosystem-fact collection and C4 derivation.")

(def Visibility
  "Publication status of a project derived from its git remote."
  [:enum :open :closed :external])

(def Layer
  "Coarse architectural stratum a project belongs to."
  [:enum :foundation :host :knowledge :storage :agent :ui :tooling :lib])

(def EcoConfig
  "The particular monorepo, as data: everything org/project-specific that the
   generic mechanism folds over. All keys optional; empty config classifies
   everything :closed/:lib (leak-safe default)."
  [:map {:closed true}
   [:dep-orgs {:optional true} [:set :string]]
   [:open-remote-patterns {:optional true} [:vector :string]]
   [:external-remote-patterns {:optional true} [:vector :string]]
   [:visibility-overrides {:optional true} [:map-of :string Visibility]]
   [:layer-table {:optional true} [:map-of :string Layer]]
   [:host-project {:optional true} :string]])

(def ProjectFact
  "Raw facts probed from one monorepo project directory."
  [:map {:closed true}
   [:name [:string {:min 1}]]
   [:dir :string]
   [:deps [:set :string]]
   [:remote [:maybe :string]]
   [:addon? :boolean]
   [:addon-manifest [:maybe :map]]])

(def ClassifiedProject
  "ProjectFact enriched with rule-chain verdicts."
  [:map {:closed true}
   [:name [:string {:min 1}]]
   [:dir :string]
   [:deps [:set :string]]
   [:remote [:maybe :string]]
   [:addon? :boolean]
   [:addon-manifest [:maybe :map]]
   [:visibility Visibility]
   [:layer Layer]])

(def EcosystemFacts
  "One probe pass over a monorepo root."
  [:map {:closed true}
   [:root :string]
   [:projects [:vector ProjectFact]]])

(def OverarchNode
  "Shape of an emitted Overarch model node."
  [:map
   [:el :keyword]
   [:id :keyword]])
