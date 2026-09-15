(ns hive-overarch.init-test
  "Addon handler boundary: the arch-* command table reads its params in both
   key shapes and threads the scope into the stores it is bound to."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.cli :as cli]
            [hive-dsl.result :as r]
            [hive-overarch.init :as init]
            [hive-overarch.orchestrator :as orch]
            [hive-overarch.protocols :as p]))

(defn- stub-system
  "OverarchSystem whose ports record every call into CALLS and return fixed values."
  [calls]
  (let [log! (fn [& entry] (swap! calls conj (vec entry)))]
    (orch/->OverarchSystem
     (reify p/IModelSource
       (derive-model [_ scope _] (log! :derive scope) (r/ok {:scope scope})))
     (reify p/IModelStore
       (put-snapshot [_ model] (log! :put (:scope model)) (r/ok {:scope (:scope model)}))
       (get-snapshot [_ id] (log! :get id) (r/ok nil))
       (list-snapshots [_ scope] (log! :list scope) (r/ok []))
       (latest [_ scope] (log! :latest scope) (r/ok {:scope scope})))
     (reify p/IViewProjection
       (project [_ model kind focus] (log! :project kind focus) (r/ok model)))
     (reify p/IRenderer
       (render-view [_ _ fmt] (log! :render fmt) (r/ok "diagram")))
     (reify p/IPersonaProjector
       (project-persona [_ model eid] (log! :persona (:scope model) eid)
         (r/ok {:prompt-fragment "persona"}))))))

(defn- invoke
  "Run command CMD of a table bound to a fresh stub system -> {:out :calls}."
  [cmd params]
  (let [calls   (atom [])
        handler (get-in (init/command-table (delay (stub-system calls))) [cmd :handler])]
    {:out (handler params) :calls @calls}))

(def ^:private key-shapes
  {"keyword keys" keyword
   "string keys"  name})

(defn- shaped [f m] (into {} (map (fn [[k v]] [(f k) v])) m))

(deftest snapshot-stamps-the-scope-into-the-store
  (doseq [[label f] key-shapes]
    (testing label
      (is (= [[:derive "hive-overarch"] [:put "hive-overarch"]]
             (:calls (invoke "arch-snapshot" (shaped f {:scope "hive-overarch"}))))))))

(deftest path-is-the-scope-fallback
  (doseq [[label f] key-shapes]
    (testing label
      (is (= [[:list "/abs/repo"]]
             (:calls (invoke "arch-list" (shaped f {:path "/abs/repo"}))))))))

(deftest render-reads-view-focus-and-format
  (doseq [[label f] key-shapes]
    (testing label
      (let [{:keys [out calls]} (invoke "arch-render"
                                        (shaped f {:scope "s" :view "container-view"
                                                   :focus "api" :format "plantuml"}))]
        (is (= "diagram" out))
        (is (= [[:latest "s"] [:project :container-view :api] [:render :plantuml]]
               calls))))))

(deftest persona-reads-element
  (doseq [[label f] key-shapes]
    (testing label
      (let [{:keys [out calls]} (invoke "arch-persona" (shaped f {:scope "s" :element "api"}))]
        (is (= "persona" out))
        (is (= [[:latest "s"] [:persona "s" :api]] calls))))))

(deftest host-cli-dispatch-reaches-the-handler-with-its-scope
  (testing "the CLI dispatcher hands contributed handlers keyword-keyed params"
    (let [calls (atom [])
          table (init/command-table (delay (stub-system calls)))
          h     (cli/make-handler {:arch-list (get-in table ["arch-list" :handler])})]
      (h {:command "arch-list" :scope "hive-overarch"})
      (is (= [[:list "hive-overarch"]] @calls)))))
