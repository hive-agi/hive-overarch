(ns hive-overarch.derive-scope-test
  "Derivation stays inside the caller's HCR scope.

   The root bug: `snapshot hive-overarch` handed the raw scope (and nil) to
   carto-search, which reads a nil scope as every indexed workspace, so the
   model was clustered from forms of unrelated projects. The source now
   canonicalizes the scope first, expands to descendants only on request, and
   drops any row outside the resulting set. Everything here runs against
   stub ports; no carto, no hive-mcp."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-overarch.derive.carto :as carto]
            [hive-overarch.hive :as hive]
            [hive-overarch.protocols :as p]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---- fixture: a carto universe that LEAKS (ignores the scope it is asked) ----

(def ^:private universe
  "Every row carto holds, across projects. Rows carry the scope they were
   indexed under, like a leaking store would."
  [{:qn "ov.core/derive"   :scope "hive-overarch"}
   {:qn "ov.core/emit"     :scope "hive-overarch"}
   {:qn "ov.core/render"   :scope "/w/hive-overarch"}
   {:qn "ov.bare/untagged"}
   {:qn "kid.core/walk"    :scope "overarch-kid"}
   {:qn "eca.core/chat"    :scope "eca"}
   {:qn "pay.core/charge"  :scope "payment-api"}])

(def ^:private foreign-qns #{"eca.core/chat" "pay.core/charge"})

(def ^:private edges-by-qn
  "Callees per qn, including edges into foreign forms."
  {"ov.core/derive" ["ov.core/emit" "eca.core/chat"]
   "ov.core/emit"   ["ov.core/render" "pay.core/charge"]
   "kid.core/walk"  ["ov.core/derive"]})

(defn- canonical-stub [sc]
  (case sc
    "/w/hive-overarch" "hive-overarch"
    sc))

(defn- ports
  "Stub ports over `universe`. :qns IGNORES its scopes argument, as the leaking
   carto-search did; the derive layer must still keep foreign rows out."
  [seen-scopes]
  {:qns         (fn [scopes] (swap! seen-scopes conj scopes) (r/ok universe))
   :callees     (fn [qn _] (r/ok (mapv (fn [to] {:qn to}) (get edges-by-qn qn))))
   :canonical   canonical-stub
   :descendants (fn [pid] (r/ok (if (= pid "hive-overarch") #{"overarch-kid"} #{})))})

(defn- capturing-clusterer
  "IElementClusterer that records the graph it was given and answers one
   cluster holding every node."
  [graph-atom]
  (reify p/IElementClusterer
    (cluster-graph [_ graph _]
      (reset! graph-atom graph)
      (r/ok [(:nodes graph)]))))

(defn derive-under
  "Derive the fixture model for `scenario` -> a plain summary. :scope is the
   Provenance scope, :nodes / :edges what the clusterer saw, :foreign the
   foreign qns that reached the graph (must be empty), :asked what carto was
   asked for."
  [{:keys [scope subtree?]}]
  (let [seen  (atom [])
        graph (atom nil)
        src   (carto/carto-source (capturing-clusterer graph) (ports seen))
        res   (p/derive-model src scope (cond-> {:min-size 1} subtree? (assoc :subtree? true)))]
    (if (r/ok? res)
      (let [nodes (:nodes @graph)]
        {:scope   (get-in (:ok res) [:provenance :scope])
         :nodes   (vec (sort nodes))
         :edges   (vec (sort (:edges @graph)))
         :foreign (vec (sort (filter foreign-qns (concat nodes (mapcat identity (:edges @graph))))))
         :asked   @seen})
      {:error (:error res)})))

(def ^:private scenarios
  {:project-id {:scope "hive-overarch"}
   :path       {:scope "/w/hive-overarch"}
   :subtree    {:scope "hive-overarch" :subtree? true}
   :nil-scope  {:scope nil}
   :blank      {:scope "  "}})

(defn derive-scenario [k] (derive-under (get scenarios k)))

(deftrifecta derive-stays-in-scope
  hive-overarch.derive-scope-test/derive-scenario
  {:golden-path "test/golden/hive-overarch/trifecta-derive-stays-in-scope.edn"
   :cases       {:project-id :project-id
                 :path       :path
                 :subtree    :subtree
                 :nil-scope  :nil-scope
                 :blank      :blank}
   :gen         (gen/elements [:project-id :path :subtree :nil-scope :blank])
   :pred        #(or (some? (:error %)) (empty? (:foreign %)))
   :mutations   [["no-filter (pre-fix)"
                  (fn [_] {:scope "hive-overarch" :foreign ["eca.core/chat"]})]]})

;; ---- the facts the golden alone does not name ----

(deftest provenance-carries-the-canonical-scope
  (testing "a path scope is stamped as its project-id (fixes the raw-scope provenance)"
    (is (= "hive-overarch" (:scope (derive-under {:scope "/w/hive-overarch"}))))))

(deftest subtree-is-opt-in
  (let [flat (derive-under {:scope "hive-overarch"})
        deep (derive-under {:scope "hive-overarch" :subtree? true})]
    (is (not-any? #{"kid.core/walk"} (:nodes flat)) "children stay out unless asked for")
    (is (some #{"kid.core/walk"} (:nodes deep)) "children join on :subtree?")
    (is (= ["hive-overarch" "overarch-kid"] (last (:asked deep))))
    (is (= ["hive-overarch"] (last (:asked flat))))))

(deftest unbounded-scope-never-reaches-carto
  (doseq [sc [nil "" "  "]]
    (let [seen (atom [])
          src  (carto/carto-source (capturing-clusterer (atom nil)) (ports seen))
          res  (p/derive-model src sc {})]
      (is (= :scope/unbounded (:error res)) (pr-str sc))
      (is (empty? @seen) "carto was not queried"))))

(deftest foreign-callee-edges-are-dropped
  (let [{:keys [edges]} (derive-under {:scope "hive-overarch"})]
    (is (= [["ov.core/derive" "ov.core/emit"] ["ov.core/emit" "ov.core/render"]] edges))))

(deftest subtree-without-a-project-tree-fails-loudly
  (let [ports' (assoc (ports (atom []))
                      :descendants (fn [_] (r/err :capability/unavailable {})))
        src    (carto/carto-source (capturing-clusterer (atom nil)) ports')
        res    (p/derive-model src "hive-overarch" {:subtree? true})]
    (is (= :capability/unavailable (:error res)))))

(deftest boundary-refuses-a-nil-scope
  (testing "hive/carto-qns answers :scope/unbounded before any capability lookup"
    (is (= :scope/unbounded (:error (hive/carto-qns nil))))
    (is (= :scope/unbounded (:error (hive/carto-qns "  "))))
    (is (= :scope/unbounded (:error (hive/carto-qns [nil ""]))))))

(deftest canonical-scope-degrades-to-the-raw-scope
  (testing "without hive-carto on the classpath the scope comes back unchanged"
    (when-not (hive/available? 'hive-carto.cartography.scope/canonical-pid)
      (is (= "/w/x" (hive/canonical-scope "/w/x"))))))
