(ns hive-overarch.hive
  "Anti-corruption boundary to the hive ecosystem (hive-knowledge / hive-mcp).
   Every capability is resolved LAZILY via requiring-resolve, so hive-overarch
   compiles and loads with only overarch + hive-dsl + hive-weave on the
   classpath, cooperates when the hive jars are present, and degrades
   gracefully when they are not. This is the ONLY namespace permitted to reach
   into hive-knowledge / hive-mcp internals. All returns are hive-dsl Results."
  (:require [hive-dsl.result :as r]
            [hive-addon.registry.commands :as addon-cmds]
            [clojure.string :as str]))

(defn- resolve-fn [sym]
  (r/rescue nil (requiring-resolve sym)))

(defn available?
  "True when the symbol resolves on the current classpath."
  [sym]
  (some? (resolve-fn sym)))

(defn- call
  "Resolve sym and invoke with args, as a Result. Missing capability is an
   :capability/unavailable error; a thrown call is :hive/call-failed."
  [sym & args]
  (if-let [f (resolve-fn sym)]
    (r/try-effect* :hive/call-failed (apply f args))
    (r/err :capability/unavailable {:symbol sym})))

;; ---- carto (hive-carto; formerly hive-knowledge) ----

(def carto-symbols
  "The symbol each carto capability of this boundary resolves. The artisan
   carto namespaces moved from hive-knowledge to hive-carto; none of the
   hive-knowledge.artisan.carto.* namespaces exist any more."
  {:carto-search   'hive-carto.artisan.carto.read/carto-search
   :carto-callees  'hive-carto.artisan.carto.read/carto-callees
   :make-clusterer 'hive-carto.artisan.carto.suggest.protocols/make-clusterer
   :cluster        'hive-carto.artisan.carto.suggest.protocols/cluster})

(def clusterer-namespaces
  "Namespaces whose defmethods register the clusterer types."
  '[hive-carto.artisan.carto.suggest.clusterers.connected
    hive-carto.artisan.carto.suggest.clusterers.greedy
    hive-carto.artisan.carto.suggest.clusterers.ns-prefix])

(defn- blank-scope?
  "True for a scope that names nothing: nil, a blank string."
  [s]
  (or (nil? s) (and (string? s) (str/blank? s))))

(defn carto-qns
  "All qualified names in scope -> Result<seq<{:qn :file :line}>>.
   `scope` is one scope string, or a collection of them: a collection goes
   to carto-search as :scopes (one multi-project query, which already
   canonicalizes each entry through the HCR alias set), a string as :scope.
   A nil or blank scope is refused with :scope/unbounded instead of being
   passed through: carto-search reads no scope as every indexed workspace."
  [scope]
  (let [multi? (sequential? scope)
        scopes (when multi? (vec (remove blank-scope? scope)))]
    (if (if multi? (empty? scopes) (blank-scope? scope))
      (r/err :scope/unbounded {:scope scope})
      (call (:carto-search carto-symbols)
            (cond-> {:ns-pattern "" :limit 10000}
              multi?       (assoc :scopes scopes)
              (not multi?) (assoc :scope scope))))))

(def project-tree-symbols
  "The symbol each HCR project-tree capability resolves. hive-mcp owns the
   project hierarchy; it is reached only through this soft-resolved seam, so
   hive-overarch never requires hive-mcp."
  {:descendant-ids 'hive-mcp.project.tree/get-descendant-ids})

(defn canonical-scope
  "Raw scope (project-id | absolute path | alias) -> canonical project-id
   string, through hive-carto's HCR scope resolver. Total: when the resolver
   is absent or throws, the raw scope comes back unchanged, so the caller
   still has a scope to bound its query with."
  [scope]
  (if-let [f (resolve-fn 'hive-carto.cartography.scope/canonical-pid)]
    (let [res (r/try-effect* :hive/call-failed (f scope))]
      (if (and (r/ok? res) (string? (:ok res)) (not (str/blank? (:ok res))))
        (:ok res)
        scope))
    scope))

(defn descendant-ids
  "Project-ids of every descendant of project-id in the HCR project tree ->
   Result<set<string>>. :capability/unavailable when hive-mcp is absent."
  [project-id]
  (call (:descendant-ids project-tree-symbols) project-id))

(defn carto-callees
  "Outgoing depends-on targets of qn -> Result<seq<{:qn ...}>>."
  [qn scope]
  (call (:carto-callees carto-symbols)
        {:qn qn :scope scope}))

(defn make-clusterer
  "Construct an Overarch-style graph clusterer of :type. Loads the impl
   namespaces first so their defmethods register the type."
  [type]
  (doseq [ns- clusterer-namespaces]
    (r/rescue nil (require ns-)))
  (call (:make-clusterer carto-symbols) {:type type}))

(defn cluster
  "Partition graph {:nodes :edges} into clusters -> Result<seq<set>>."
  [clusterer graph opts]
  (if-let [f (resolve-fn (:cluster carto-symbols))]
    (r/try-effect* :carto/cluster-failed (f clusterer graph opts))
    (r/err :capability/unavailable {:symbol (:cluster carto-symbols)})))

;; ---- memory store (hive-mcp) ----

(defn- mem-store []
  (when-let [g (resolve-fn 'hive-mcp.protocols.memory/get-store)]
    (r/rescue nil (g))))

(defn memory-add!
  "Persist a memory entry map -> Result<id>."
  [entry]
  (if-let [add (resolve-fn 'hive-mcp.protocols.memory/add-entry!)]
    (if-let [store (mem-store)]
      (r/try-effect* :memory/add-failed (add store entry))
      (r/err :capability/unavailable {:symbol :memory-store}))
    (r/err :capability/unavailable {:symbol 'hive-mcp.protocols.memory/add-entry!})))

(defn memory-query
  "Query memory entries by opts -> Result<seq<entry>>."
  [opts]
  (if-let [q (resolve-fn 'hive-mcp.protocols.memory/query-entries)]
    (if-let [store (mem-store)]
      (r/try-effect* :memory/query-failed (q store opts))
      (r/err :capability/unavailable {:symbol :memory-store}))
    (r/err :capability/unavailable {:symbol 'hive-mcp.protocols.memory/query-entries})))

;; ---- ephemeral context store (hive-mcp) — backs persona ctx-refs ----

(defn context-put!
  "Stash data in the ephemeral context-store -> Result<ctx-id>. ~5-min TTL."
  [data tags]
  (call 'hive-mcp.channel.context-store/context-put! data :tags (set tags)))

;; ---- MCP command contribution (hive-mcp) — used by the addon at init ----

(defn- contributed-tree
  "The full contributed-command tree across every tool, {tool-name {cmd-name cmd-spec}}.
   Mirrors hive-mcp.extensions.registry's all-contributions, rebuilt from the
   hive-addon store so callers still see the whole tree, not just the names
   this call touched."
  []
  (into {} (map (juxt identity addon-cmds/get-commands)) (addon-cmds/contributed-tool-names)))

(defn contribute-commands!
  "Register subcommands for a composite MCP tool -> Result."
  [tool-name addon-id commands]
  (r/try-effect* :hive/call-failed
    (addon-cmds/contribute! tool-name addon-id commands)
    (contributed-tree)))

(defn retract-commands!
  "Reclaim all commands contributed by addon-id -> Result."
  [addon-id]
  (r/try-effect* :hive/call-failed
    (addon-cmds/retract-all! addon-id)
    (contributed-tree)))

;; ---- embedding policy (hive-mcp) — declare structurally-addressed types ----

(defn declare-non-semantic-type!
  "Tell the embedding service that entries of `type-str` are fetched only
   structurally (tag/id/project-id), so the write path skips embedding them.
   No-op (graceful) when hive-mcp is absent. -> Result."
  [type-str]
  (call 'hive-mcp.embeddings.service/register-no-embed-type! type-str))
