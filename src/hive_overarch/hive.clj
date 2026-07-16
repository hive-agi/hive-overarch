(ns hive-overarch.hive
  "Anti-corruption boundary to the hive ecosystem (hive-knowledge / hive-mcp).
   Every capability is resolved LAZILY via requiring-resolve, so hive-overarch
   compiles and loads with only overarch + hive-dsl + hive-weave on the
   classpath, cooperates when the hive jars are present, and degrades
   gracefully when they are not. This is the ONLY namespace permitted to reach
   into hive-knowledge / hive-mcp internals. All returns are hive-dsl Results."
  (:require [hive-dsl.result :as r]))

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

;; ---- carto (hive-knowledge) ----

(defn carto-qns
  "All qualified names in scope -> Result<seq<{:qn :file :line}>>."
  [scope]
  (call 'hive-knowledge.artisan.carto.read/carto-search
        {:ns-pattern "" :scope scope :limit 10000}))

(defn carto-callees
  "Outgoing depends-on targets of qn -> Result<seq<{:qn ...}>>."
  [qn scope]
  (call 'hive-knowledge.artisan.carto.read/carto-callees
        {:qn qn :scope scope}))

(defn make-clusterer
  "Construct an Overarch-style graph clusterer of :type. Loads the impl
   namespaces first so their defmethods register the type."
  [type]
  (doseq [ns- '[hive-knowledge.artisan.carto.suggest.connected
                hive-knowledge.artisan.carto.suggest.greedy
                hive-knowledge.artisan.carto.suggest.ns-prefix]]
    (r/rescue nil (require ns-)))
  (call 'hive-knowledge.artisan.carto.suggest.protocols/make-clusterer {:type type}))

(defn cluster
  "Partition graph {:nodes :edges} into clusters -> Result<seq<set>>."
  [clusterer graph opts]
  (if-let [f (resolve-fn 'hive-knowledge.artisan.carto.suggest.protocols/cluster)]
    (r/try-effect* :carto/cluster-failed (f clusterer graph opts))
    (r/err :capability/unavailable {:symbol 'hive-knowledge.artisan.carto.suggest.protocols/cluster})))

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

(defn contribute-commands!
  "Register subcommands for a composite MCP tool -> Result."
  [tool-name addon-id commands]
  (call 'hive-mcp.extensions.registry/contribute-commands! tool-name addon-id commands))

(defn retract-commands!
  "Reclaim all commands contributed by addon-id -> Result."
  [addon-id]
  (call 'hive-mcp.extensions.registry/retract-all-by-addon! addon-id))

;; ---- embedding policy (hive-mcp) — declare structurally-addressed types ----

(defn declare-non-semantic-type!
  "Tell the embedding service that entries of `type-str` are fetched only
   structurally (tag/id/project-id), so the write path skips embedding them.
   No-op (graceful) when hive-mcp is absent. -> Result."
  [type-str]
  (call 'hive-mcp.embeddings.service/register-no-embed-type! type-str))
