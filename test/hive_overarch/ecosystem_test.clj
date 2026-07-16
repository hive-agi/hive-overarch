(ns hive-overarch.ecosystem-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [malli.core :as mc]
            [malli.generator :as mg]
            [hive-dsl.result :as r]
            [hive-overarch.ecosystem.classify :as classify]
            [hive-overarch.ecosystem.derive :as eco]
            [hive-overarch.ecosystem.emit :as emit]
            [hive-overarch.ecosystem.probe :as probe]
            [hive-overarch.ecosystem.schema :as s]
            [hive-overarch.model :as m]
            [hive-overarch.orchestrator :as orch]
            [hive-overarch.protocols :as p]
            [hive-overarch.derive.ecosystem :as source]))

;; ---- the particular monorepo as data (EcoConfig double) ----

(def test-config
  {:dep-orgs #{"io.github.hive-agi"}
   :open-remote-patterns ["github\\.com[:/](hive-agi|BuddhiLW)/"]
   :external-remote-patterns ["github\\.com"]
   :visibility-overrides {"mirror-lib" :open}
   :host-project "hive-mcp"
   :layer-table {"open-lib" :foundation "hive-mcp" :host "open-app" :agent}})

;; ---- synthetic monorepo (fs seam double — no real filesystem) ----

(def fake-tree
  {"/repo/open-lib/deps.edn"    (pr-str {:deps {'org.clojure/clojure {:mvn/version "1.12.1"}}})
   "/repo/open-lib/.git/config" "[remote \"origin\"]\n\turl = git@github.com:hive-agi/open-lib.git\n"
   "/repo/open-app/deps.edn"    (pr-str {:deps {'io.github.hive-agi/open-lib {:git/tag "v1"}}
                                         :aliases {:dev {:extra-deps {'io.github.hive-agi/hive-mcp {:git/tag "v1"}}}}})
   "/repo/open-app/.git/config" "[remote \"origin\"]\n\turl = https://github.com/hive-agi/open-app.git\n"
   "/repo/open-app/resources/META-INF/hive-addons/open-app.edn"
   (pr-str {:addon/id "open-app" :addon/description "an addon"})
   "/repo/hive-mcp/deps.edn"    (pr-str {:deps {'io.github.hive-agi/open-lib {:git/tag "v1"}}})
   "/repo/hive-mcp/.git/config" "[remote \"origin\"]\n\turl = git@github.com:hive-agi/hive-mcp.git\n"
   "/repo/secret/deps.edn"      (pr-str {:deps {'io.github.hive-agi/open-lib {:git/tag "v1"}}})
   "/repo/secret/.git/config"   "[remote \"origin\"]\n\turl = git@gitea.example.com:x/secret.git\n"
   "/repo/mirror-lib/deps.edn"  (pr-str {:deps {}})
   "/repo/mirror-lib/.git/config" "[remote \"origin\"]\n\turl = git@gitea.example.com:x/mirror-lib.git\n"
   "/repo/vendored/deps.edn"    (pr-str {:deps {}})
   "/repo/vendored/.git/config" "[remote \"origin\"]\n\turl = https://github.com/elsewhere/vendored.git\n"
   "/repo/no-deps/README.md"    "not a project"})

(def fake-fs
  (let [paths (keys fake-tree)
        under (fn [dir] (let [prefix (str dir "/")]
                          (->> paths
                               (filter #(str/starts-with? % prefix))
                               (map #(str prefix (first (str/split
                                                         (subs % (count prefix)) #"/"))))
                               distinct)))]
    {:list-dirs  (fn [dir] (remove fake-tree (under dir)))
     :list-files (fn [dir] (filter fake-tree (under dir)))
     :read-file  (fn [path] (get fake-tree path))}))

(defn- facts [] (:ok (probe/collect-facts "/repo" {:fs fake-fs :config test-config})))
(defn- model [] (eco/facts->model (facts) {:config test-config}))

;; ---- probe ----

(deftest collect-facts-shape
  (let [{:keys [projects] :as f} (facts)]
    (is (mc/validate s/EcosystemFacts f))
    (is (= #{"open-lib" "open-app" "hive-mcp" "secret" "mirror-lib" "vendored"}
           (into #{} (map :name) projects)))
    (testing "deps.edn coordinates seen across :deps and aliases"
      (is (= #{"open-lib" "hive-mcp"}
             (:deps (first (filter #(= "open-app" (:name %)) projects))))))
    (testing "addon manifest probed"
      (is (:addon? (first (filter #(= "open-app" (:name %)) projects))))
      (is (not (:addon? (first (filter #(= "open-lib" (:name %)) projects))))))))

(deftest parse-remote-url-cases
  (is (= "git@github.com:hive-agi/x.git"
         (probe/parse-remote-url "[remote \"origin\"]\n\turl = git@github.com:hive-agi/x.git")))
  (is (nil? (probe/parse-remote-url nil)))
  (is (nil? (probe/parse-remote-url "no remotes here"))))

;; ---- classify: config-driven rules + rule order ----

(deftest visibility-table
  (letfn [(fact [nm remote] {:name nm :dir "/x" :deps #{} :remote remote
                             :addon? false :addon-manifest nil})]
    (is (= :open     (classify/visibility test-config (fact "x" "git@github.com:hive-agi/x.git"))))
    (is (= :open     (classify/visibility test-config (fact "x" "https://github.com/BuddhiLW/y.git"))))
    (is (= :external (classify/visibility test-config (fact "x" "https://github.com/elsewhere/z.git"))))
    (is (= :closed   (classify/visibility test-config (fact "x" "git@gitea.example.com:x/s.git"))))
    (is (= :closed   (classify/visibility test-config (fact "x" nil))))
    (testing "override beats remote pattern (rule order)"
      (is (= :open (classify/visibility test-config
                                        (fact "mirror-lib" "git@gitea.example.com:x/m.git")))))
    (testing "empty config is leak-safe: everything :closed"
      (is (= :closed (classify/visibility {} (fact "x" "git@github.com:hive-agi/x.git")))))))

(defspec visibility-total 50
  (prop/for-all [f (mg/generator s/ProjectFact)]
    (contains? #{:open :closed :external} (classify/visibility test-config f))))

(defspec layer-total 50
  (prop/for-all [f (mg/generator s/ProjectFact)]
    (mc/validate s/Layer (classify/layer test-config f))))

(defspec classify-project-valid 50
  (prop/for-all [f (mg/generator s/ProjectFact)]
    (mc/validate s/ClassifiedProject (classify/classify-project test-config f))))

(deftest classify-rules-are-open
  (testing "a new rule wins without editing the fold (OCP)"
    (let [rules (cons (classify/rule :force-open (constantly true) :open)
                      (classify/visibility-rules test-config))]
      (is (= :open (classify/classify rules :closed
                                      {:name "x" :dir "/x" :deps #{}
                                       :remote nil :addon? false
                                       :addon-manifest nil}))))))

;; ---- derive ----

(deftest facts->model-elements-and-relations
  (let [model (model)
        ids   (into #{} (map :id) (:elements model))
        rels  (into #{} (map (juxt :from :to :name)) (:relations model))]
    (is (= #{:hive/open-lib :hive/open-app :hive/hive-mcp :hive/secret
             :hive/mirror-lib :hive/vendored} ids))
    (is (contains? rels [:hive/open-app :hive/open-lib "depends on"]))
    (is (contains? rels [:hive/open-app :hive/hive-mcp "plugs into"]))
    (testing "tags carry visibility + layer + iaddon"
      (let [tag-of (fn [id] (:tags (first (filter #(= id (:id %)) (:elements model)))))]
        (is (contains? (tag-of :hive/open-app) "open"))
        (is (contains? (tag-of :hive/open-app) "iaddon"))
        (is (contains? (tag-of :hive/secret) "closed"))
        (is (contains? (tag-of :hive/mirror-lib) "open"))
        (is (contains? (tag-of :hive/vendored) "external"))))))

(deftest restrict-open-excludes-closed
  (let [open (eco/restrict-open (model))
        ids  (into #{} (map :id) (:elements open))]
    (is (= #{:hive/open-lib :hive/open-app :hive/hive-mcp :hive/mirror-lib} ids))
    (testing "no relation endpoint may reference an excluded project"
      (is (every? (fn [{:keys [from to]}] (and (ids from) (ids to)))
                  (:relations open))))))

(deftest mindmap-groups-by-layer
  (let [mind (eco/mindmap-model (model))
        ids  (into #{} (map :id) (:elements mind))]
    (is (contains? ids :hive.mind/ecosystem))
    (is (contains? ids :hive.mind/layer-host))
    (is (contains? ids :hive.mind/hive-mcp))
    (testing "every relation endpoint exists"
      (is (every? (fn [{:keys [from to]}] (and (ids from) (ids to)))
                  (:relations mind))))))

;; ---- emit: round-trip as valid EDN with overarch shape ----

(deftest emitted-files-are-valid-overarch-edn
  (let [files (emit/model-files (model) "full")
        header ";; generated by hive-overarch.ecosystem.emit — do not hand-edit"
        model-set (edn/read-string (subs (get files "model.edn") (count header)))
        views-set (edn/read-string (subs (get files "views.edn") (count header)))]
    (is (set? model-set))
    (is (every? #(and (:el %) (:id %)) model-set))
    (is (set? views-set))
    (is (contains? (into #{} (map :id) views-set) :hive.views/landscape-full))
    (is (contains? (into #{} (map :id) views-set) :hive.iaddon/lifecycle-view))))

(deftest emit!-writes-open-and-full
  (let [written (atom {})
        res     (emit/emit! (model) "/out" {:write-fn (fn [p b] (swap! written assoc p b))})]
    (is (r/ok? res))
    (is (= #{"/out/ecosystem-full/model.edn" "/out/ecosystem-full/views.edn"
             "/out/ecosystem-open/model.edn" "/out/ecosystem-open/views.edn"}
           (set (keys @written))))
    (testing "the open artifact never mentions a closed project"
      (is (not (re-find #"secret" (get @written "/out/ecosystem-open/model.edn")))))))

;; ---- emit: layer nesting via :ct ----

(deftest layered-nodes-group-projects-by-layer
  (let [nodes      (emit/layered-element-nodes (:elements (model)))
        boundaries (filter #(= :context-boundary (:el %)) nodes)
        bids       (into #{} (map :id) boundaries)
        children   (fn [bid] (into #{} (map :id)
                                   (:ct (first (filter #(= bid (:id %)) boundaries)))))]
    (is (contains? bids :hive/layer-foundation))
    (is (contains? bids :hive/layer-host))
    (is (contains? bids :hive/layer-lib))
    (is (contains? (children :hive/layer-host) :hive/hive-mcp))
    (is (contains? (children :hive/layer-foundation) :hive/open-lib))
    (testing "every project node is nested, none left top-level"
      (is (not-any? #(= :system (:el %)) nodes)))))

;; ---- IModelStore: eco snapshots persist like carto snapshots ----

(deftest eco-snapshot-persists-through-imodelstore
  (let [saved (atom nil)
        store (reify p/IModelStore
                (put-snapshot [_ model]
                  (reset! saved model)
                  (let [{:keys [snapshot-id scope taken-at]} (:provenance model)]
                    (r/ok (m/->SnapshotRef snapshot-id scope taken-at
                                           (count (:elements model)))))))
        sys   {:source (source/ecosystem-source {:fs fake-fs :config test-config})
               :store  store}
        res   (orch/snapshot! sys "/repo" {})]
    (is (r/ok? res))
    (is (= 6 (:element-count (:ok res))))
    (is (str/starts-with? (:id (:ok res)) "eco-"))
    (testing "the persisted value is the full model with provenance"
      (is (= :ecosystem (:source (:provenance @saved))))
      (is (some? (:taken-at (:provenance @saved)))))))

;; ---- config loading ----

(deftest load-config-cases
  (let [cfg-fs {:read-file {"/cfg/good.edn" (pr-str test-config)
                            "/cfg/bad.edn"  "{:layer-table 42}"}}]
    (is (= test-config (:ok (source/load-config "/cfg/good.edn" {:fs cfg-fs}))))
    (is (r/err? (source/load-config "/cfg/missing.edn" {:fs cfg-fs})))
    (is (r/err? (source/load-config "/cfg/bad.edn" {:fs cfg-fs})))))

(deftest emit-from-config-end-to-end
  (let [written (atom {})
        fs      (update fake-fs :read-file
                        (fn [rf] (fn [p] (if (= "/cfg/eco.edn" p) (pr-str test-config) (rf p)))))
        res     (source/emit-from-config!
                 {:root "/repo" :out "/out" :config-path "/cfg/eco.edn" :fs fs
                  :write-fn (fn [p b] (swap! written assoc p b))})]
    (is (r/ok? res))
    (is (= 4 (count @written)))
    (is (not (re-find #"secret" (get @written "/out/ecosystem-open/model.edn"))))
    (testing "a bad config path is an error, not a silent closed-everything"
      (is (r/err? (source/emit-from-config!
                   {:root "/repo" :config-path "/cfg/nope.edn" :fs fs
                    :write-fn (fn [_ _])}))))))

;; ---- IModelSource seam ----

(deftest ecosystem-source-implements-imodelsource
  (let [src (source/ecosystem-source {:fs fake-fs :config test-config})
        res (p/derive-model src "/repo" {})]
    (is (r/ok? res))
    (let [model (:ok res)]
      (is (= :ecosystem (:source (:provenance model))))
      (is (= 6 (count (:elements model)))))))
