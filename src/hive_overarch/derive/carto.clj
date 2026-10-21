(ns hive-overarch.derive.carto
  "IModelSource that derives a C4 model SNAPSHOT from the carto code-graph:
   enumerate qns -> build the qn dependency graph (parallel reads) -> cluster
   into modules -> emit each cluster as a C4 :system element with inter-module
   :depends-on relations. The result is an immutable, provenance-stamped value."
  (:require [clojure.string :as str]
            [hive-weave.parallel :as wp]
            [hive-overarch.protocols :as p]
            [hive-overarch.hive :as hive]
            [hive-overarch.model :as m]
            [hive-dsl.result :as r]))

(defn- qn->ns [qn]
  (first (str/split (str qn) #"/" 2)))

(defn- common-ns-prefix [namespaces]
  (let [splits (map #(str/split % #"\.") namespaces)]
    (cond
      (empty? splits)       "module"
      (= 1 (count splits))  (first namespaces)
      :else
      (let [cols   (apply map vector splits)
            common (take-while #(apply = %) cols)]
        (if (seq common)
          (let [common-strs (map first common)]
            (if (and (= 1 (count common-strs))
                     (some #(>= (count %) 2) splits))
              (let [second-segs (keep #(when (>= (count %) 2) (nth % 1)) splits)
                    freqs       (frequencies second-segs)
                    max-freq    (apply max (vals freqs))
                    dominant    (->> (filter #(= max-freq (val %)) freqs)
                                    (map key)
                                    sort
                                    first)
                    k           (dec (count (distinct second-segs)))]
                (if (zero? k)
                  (str (first common-strs) "." dominant)
                  (str (first common-strs) "." dominant "+" k)))
              (str/join "." common-strs)))
          (first namespaces))))))

(defn- cluster->element [idx qns]
  (let [namespaces (distinct (map qn->ns qns))
        nm         (common-ns-prefix namespaces)
        id         (keyword "c4" (str (str/replace nm #"[./]" "-") "-" idx))]
    (m/element id :system nm
               :tech "Clojure"
               :description (str (count qns) " forms across " (count namespaces) " namespace(s)")
               :carto-refs qns)))

(defn- callees-in-scopes
  "Outgoing depends-on targets of qn, read through `callees` ((qn scope) ->
   Result) under each of scopes in order; the first scope that answers a
   non-empty ok wins. nil when none does."
  [callees qn scopes]
  (some (fn [sc]
          (let [res (callees qn sc)]
            (when (and res (r/ok? res) (seq (:ok res))) res)))
        scopes))

(defn- qn-edges
  "Parallel carto reads -> set of [from-qn to-qn] depends-on pairs. Only edges
   whose BOTH ends are in `qns` survive: a callee outside the scope set is a
   foreign form, and the derived graph must not carry it."
  [callees qns scopes]
  (let [in-set  (set qns)
        results (wp/bounded-pmap {:concurrency 8 :timeout-ms 15000 :fallback nil}
                                 (fn [qn] (callees-in-scopes callees qn scopes))
                                 qns)]
    (into #{}
          (mapcat (fn [qn res]
                    (when res
                      (for [c   (:ok res)
                            :let [to (:qn c)]
                            :when (and to (contains? in-set to))]
                        [qn to])))
                  qns results))))

(defn- cluster-relations [edges qn->eid]
  (->> edges
       (keep (fn [[from to]]
               (let [ef (qn->eid from) et (qn->eid to)]
                 (when (and ef et (not= ef et)) [ef et]))))
       distinct
       (map-indexed (fn [i [ef et]]
                      (m/relation (keyword "c4-rel" (str "r" i)) ef et :name "depends on")))))

(def default-ports
  "The carto/HCR capabilities derivation reads through, as functions, all
   resolved at call time through the hive boundary:
     :qns         scope-or-scopes -> Result<seq<{:qn ..}>>
     :callees     qn scope        -> Result<seq<{:qn ..}>>
     :canonical   raw-scope       -> canonical project-id string
     :descendants project-id      -> Result<set<project-id>>
     :scan-id     project-id      -> carto's latest scan id, or nil
   Tests substitute stubs for these; no redefinition of the boundary."
  {:qns         (fn [scope] (hive/carto-qns scope))
   :callees     (fn [qn scope] (hive/carto-callees qn scope))
   :canonical   (fn [scope] (hive/canonical-scope scope))
   :descendants (fn [pid] (hive/descendant-ids pid))
   :scan-id     (fn [pid] (hive/carto-scan-id pid))})

(defn- blank-scope? [s]
  (or (nil? s) (and (string? s) (str/blank? s))))

(defn scope-plan
  "Pure given its ports. Resolve the caller's raw scope (project-id | path |
   alias) into the bounded set derivation may read ->
   Result<{:pid canonical-id :scopes [pid & descendant-ids]}>.

   The scope is canonicalized FIRST (HCR cartography.scope), so the model's
   Provenance carries a real project-id rather than whatever spelling the
   caller typed. Descendants join only when `:subtree?` is set: expanding to
   child projects is an explicit request, and one that cannot be honoured
   (no project tree) fails instead of silently narrowing. A nil or blank
   scope is refused with :scope/unbounded: handed to carto it reads as every
   indexed workspace."
  [{:keys [canonical descendants]} scope {:keys [subtree?]}]
  (if (blank-scope? scope)
    (r/err :scope/unbounded {:scope scope})
    (let [pid (canonical scope)]
      (if (blank-scope? pid)
        (r/err :scope/unbounded {:scope scope})
        (if subtree?
          (r/let-ok [kids (descendants pid)]
            (r/ok {:pid pid :scopes (into [pid] (sort (remove #{pid} kids)))}))
          (r/ok {:pid pid :scopes [pid]}))))))

(defn rows-in-scope
  "Defensive filter over carto rows. A row that names its own scope (:scope or
   :project-id) must canonicalize into `allowed` (a set of project-ids); a
   row naming none is kept, as carto-search rows carry no scope of their own
   and were already bounded by the query. Rows without a :qn are dropped."
  [canonical allowed rows]
  (filterv (fn [row]
             (and (:qn row)
                  (let [own (or (:scope row) (:project-id row))]
                    (or (blank-scope? own)
                        (contains? allowed (canonical own))))))
           rows))

(defn- live-scan-id
  "The carto scan id to stamp on a snapshot of `pid`, read through the
   :scan-id port. nil when the port is absent, answers nothing, or throws: a
   snapshot without lineage is still a snapshot."
  [ports pid]
  (when-let [f (:scan-id ports)]
    (r/rescue nil (f pid))))

(defrecord CartoDerivedSource [clusterer ports]
  p/IModelSource
  (derive-model [_ scope opts]
    (let [{:keys [qns callees canonical]} ports]
      (r/let-ok [plan    (scope-plan ports scope opts)
                 qn-rows (qns (:scopes plan))
                 :let [pid       (:pid plan)
                       scopes    (:scopes plan)
                       qn-list   (vec (keep :qn (rows-in-scope canonical (set scopes) qn-rows)))
                       edges     (qn-edges callees qn-list scopes)
                       graph     {:nodes (set qn-list) :edges edges}]
                 clusters (p/cluster-graph clusterer graph
                                           {:min-size (:min-size opts 3)
                                            :max-size (:max-size opts 12)})]
        (let [clusters (vec clusters)
              elements (vec (map-indexed cluster->element clusters))
              qn->eid  (into {} (mapcat (fn [el qset] (map (fn [q] [q (:id el)]) qset))
                                        elements clusters))
              relations (vec (cluster-relations edges qn->eid))
              prov (m/->Provenance (str "c4-" (System/currentTimeMillis))
                                   pid (str (java.time.Instant/now))
                                   (live-scan-id ports pid)
                                   :carto (:type clusterer))]
          (r/ok (m/model elements relations prov)))))))

(defn carto-source
  "CartoDerivedSource over `clusterer`. `ports` defaults to `default-ports`
   (the live hive boundary); a partial map overrides just those entries."
  ([clusterer] (carto-source clusterer nil))
  ([clusterer ports] (->CartoDerivedSource clusterer (merge default-ports ports))))
