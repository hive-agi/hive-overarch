# hive-overarch

Architecture-as-data for the hive ecosystem. Derives **C4 model snapshots** from
the carto code-graph, persists them in hive memory/KG, renders them to diagrams
via [Overarch](https://github.com/soulspace-org/overarch), and projects
**bounded-context personas** to prime spawned swarm agents.

C4 is the *architectural altitude* above carto's *form altitude* — the same graph
at two zoom levels. hive-overarch is the bridge.

## Snapshot semantics (the keystone)

A C4 model is an **immutable value taken at a point in time**, carrying its own
`Provenance`. It never claims to be current; there is no in-place update — you
take a *new* snapshot. "Current" is just "latest snapshot". Staleness is
*observable* (compare a snapshot's carto scan to the live one) but never an
*obligation*. A snapshot that cannot mutate cannot silently drift — which is why
there is no reconcile/dual-sync machinery here.

## SOLID seams (`hive-overarch.protocols`)

| Protocol            | Default adapter                         |
|---------------------|-----------------------------------------|
| `IModelSource`      | `derive.carto/CartoDerivedSource`       |
| `IElementClusterer` | `derive.cluster/CartoClusterer`         |
| `IModelStore`       | `store.kg/KgModelStore`                 |
| `IViewProjection`   | `view/DefaultViewProjection`            |
| `IRenderer`         | `render.overarch/OverarchRenderer`      |
| `IPersonaProjector` | `persona/SubgraphPersonaProjector`      |

Orchestration (`orchestrator`) depends only on the protocols; adapters are
injected at the composition root (`make-system`). Adding a renderer, clusterer,
or source is a new record — zero pipeline change (OCP).

## How it plays with hive-knowledge

Runtime deps are **overarch + hive-dsl + hive-weave only**. Every
hive-knowledge / hive-mcp capability (carto reads, clustering, memory store,
KG, context-store, MCP command contribution) is resolved **lazily via
`requiring-resolve`** through one anti-corruption boundary: `hive-overarch.hive`.

- Loaded inside a hive JVM → the gateway resolves and everything works.
- Standalone → the pure core (model / view / render / persona prompt) still
  works; hive-dependent paths return graceful `:capability/unavailable` Results.

This also keeps the addon free of any compile-time coupling to (proprietary)
hive-knowledge.

## Addon

Ships `resources/META-INF/hive-addons/hive-overarch.edn`. hive-mcp discovers it
on the classpath and calls the zero-arg `hive-overarch.init/init-as-addon!`,
which reify-implements `IAddon` and contributes four subcommands at
`initialize!`:

```
overarch snapshot --scope <id>      ; derive + persist a C4 snapshot
overarch render   --scope <id> [--view <kind>] [--focus <id>]
overarch persona  --scope <id> --element <c4-id>
overarch list     --scope <id>
arch-eco-emit --root <monorepo> [--out <dir>] [--config <edn-path> | --config-edn <edn>]
```

> **One host change to surface the tool:** a brand-new top-level composite tool
> needs its name whitelisted in `hive-mcp.extensions.loader` —
> `(composite/build-all-composite-tools {"analysis" "Code analysis"
> "overarch" "Architecture model"})`. The addon already contributes the commands
> (reclaimed on shutdown); the whitelist line makes them a standalone MCP tool.

## Ecosystem source (monorepo facts -> C4 + mind-map)

`hive-overarch.derive.ecosystem/EcosystemSource` is a second `IModelSource`
that derives a snapshot from **monorepo facts** instead of carto: deps.edn
sibling coordinates become relations, git remotes become visibility tags
(`open`/`closed`/`external`), IAddon manifests become `iaddon` tags +
"plugs into" relations. Derivation is millisecond-cheap, so snapshots are
retaken instead of maintained.

The mechanism is generic; the **particular** monorepo (dep orgs, remote
patterns, layer table, visibility overrides, host project) arrives as an
`EcoConfig` map — from an untracked EDN file (see `dev/hive-eco-config.edn`,
gitignored) or inline through the MCP call (`config-edn`). Empty config
classifies everything `:closed`/`:lib` (leak-safe).

`arch-eco-emit` writes two Overarch model dirs: `ecosystem-full` and
`ecosystem-open` (open-tagged elements only — never mentions closed
projects). Projects are nested into one `:context-boundary` per layer, so
landscape views group by stratum. Each dir contains landscape + addon-fleet +
concept mind-map views plus the curated IAddon-pattern container/lifecycle
views. Pass `persist=true` to also store the full model as an immutable
`c4-snapshot` through `IModelStore` (KG-backed when inside a hive JVM).

Render with the upstream CLI:

```bash
clojure -M:overarch-cli -m models/ecosystem-full -r plantuml   # or graphviz
```

Or run the whole pipeline (emit → render → rasterize → optional sync):

```bash
HIVE_ECO_ROOT=.. HIVE_ECO_CONFIG=dev/my-eco-config.edn \
HIVE_ECO_DOCS=../docs/diagrams bb regen-docs
```

## v1 derivation

Each carto namespace-cluster becomes a C4 `:system` element (renders cleanly
with no parent-boundary requirement); inter-cluster `depends-on` edges become
relations; the default view is `:system-landscape-view`. Future work: promote to
`system → container → component` nesting via `:ct`, and project nodes/edges into
the KG with a registered `:c4/contains` relation.

## Status / verification

- Pure core (model, view, render, persona prompt) — unit + Overarch integration
  tested (`clojure -M:test`).
- **carto derivation (`derive.carto` / `hive.clj`) needs live-REPL verification**
  against hive-knowledge: the carto read-fn signatures
  (`carto-search` / `carto-callees` / `make-clusterer` / `cluster`) are grounded
  in source but were not executed against a running carto. Run under
  `-A:with-hive` inside a hive JVM and confirm `orchestrator/snapshot!` round-trips.

## Develop

```bash
clojure -M:test                 # unit + Overarch render integration
clojure -A:dev                  # REPL (see dev/user.clj)
clojure -A:dev:with-hive        # REPL with hive jars for the live carto path
```
