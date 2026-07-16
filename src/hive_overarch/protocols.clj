(ns hive-overarch.protocols
  "The seams (ports) of hive-overarch. Narrow protocols (ISP), each independently
   substitutable (LSP). Orchestration depends on these; concrete records are
   injected at the composition root (DIP).")

(defprotocol IModelSource
  "Produce an immutable C4Model snapshot from some substrate."
  (derive-model [this scope opts] "scope+opts -> Result<C4Model>"))

(defprotocol IElementClusterer
  "Group a code-graph {:nodes :edges} into clusters. Strategy seam (OCP)."
  (cluster-graph [this graph opts] "-> Result<seq<set<node>>>"))

(defprotocol IModelStore
  "Persist and recall immutable model snapshots."
  (put-snapshot   [this model] "-> Result<SnapshotRef>")
  (get-snapshot   [this id]    "-> Result<C4Model>")
  (list-snapshots [this scope] "-> Result<seq<SnapshotRef>>")
  (latest         [this scope] "-> Result<C4Model>"))

(defprotocol IViewProjection
  "Project a full model down to one C4 view. Pure."
  (project [this model view-kind focus-id] "-> Result<C4View>"))

(defprotocol IRenderer
  "Render a C4 view to an external diagram format."
  (render-view [this view fmt] "-> Result<String>"))

(defprotocol IPersonaProjector
  "Project the subgraph around a C4 element into spawn-ready persona context."
  (project-persona [this model element-id] "-> Result<PersonaContext>"))
