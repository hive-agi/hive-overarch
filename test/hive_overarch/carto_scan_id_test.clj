(ns hive-overarch.carto-scan-id-test
  "A snapshot's lineage to the carto graph: derive stamps Provenance
   :carto-scan-id from the live carto scan, `hive/carto-scan-id` soft-resolves
   it (nil when hive-carto is absent), and `model/stale?` compares a stored
   snapshot against the live scan. Stub ports and an injected scan-state
   value only; no carto, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-overarch.derive.carto :as carto]
            [hive-overarch.hive :as hive]
            [hive-overarch.model :as m]
            [hive-overarch.protocols :as p]
            [hive-test.trifecta :refer [deftrifecta]]))

;; ---- hive/carto-scan-id ----------------------------------------------------

(def ^:private states
  {"hive-overarch" {:scan-id "9b1c-scan" :status :done}
   "blank-id"      {:scan-id "  "}
   "no-id"         {:status :queued}})

(deftest scan-id-reads-the-scope-entry-of-carto-scan-state
  (is (= "9b1c-scan" (hive/carto-scan-id "hive-overarch" (atom states))))
  (testing "a value as well as a ref"
    (is (= "9b1c-scan" (hive/carto-scan-id "hive-overarch" states)))))

(deftest scan-id-is-nil-when-there-is-nothing-to-read
  (is (nil? (hive/carto-scan-id "never-scanned" (atom states))))
  (is (nil? (hive/carto-scan-id "blank-id" (atom states))))
  (is (nil? (hive/carto-scan-id "no-id" (atom states))))
  (is (nil? (hive/carto-scan-id nil (atom states))) "a blank scope names nothing")
  (is (nil? (hive/carto-scan-id "hive-overarch" nil)) "no scan state at all")
  (is (nil? (hive/carto-scan-id "hive-overarch" (atom :not-a-map)))))

(deftest scan-id-soft-resolves-without-hive-carto
  (testing "hive-carto is not on this classpath: nil, never a throw"
    (is (nil? (hive/carto-scan-id "hive-overarch")))))

;; ---- derive stamps it ------------------------------------------------------

(defn- one-cluster []
  (reify p/IElementClusterer
    (cluster-graph [_ graph _] (r/ok [(:nodes graph)]))))

(defn- ports [scan-id-fn]
  {:qns         (fn [_] (r/ok [{:qn "ov.core/derive"} {:qn "ov.core/emit"}]))
   :callees     (fn [_ _] (r/ok []))
   :canonical   identity
   :descendants (fn [_] (r/ok #{}))
   :scan-id     scan-id-fn})

(deftest derive-stamps-the-live-scan-id-of-the-canonical-scope
  (let [asked (atom nil)
        src   (carto/carto-source (one-cluster) (ports (fn [pid] (reset! asked pid) "scan-42")))
        res   (p/derive-model src "hive-overarch" {:min-size 1})]
    (is (r/ok? res))
    (is (= "scan-42" (get-in res [:ok :provenance :carto-scan-id])))
    (is (= "hive-overarch" @asked) "asked for the canonical project-id")))

(deftest derive-stamps-nil-when-carto-has-no-scan
  (let [res (p/derive-model (carto/carto-source (one-cluster) (ports (constantly nil)))
                            "hive-overarch" {:min-size 1})]
    (is (r/ok? res))
    (is (nil? (get-in res [:ok :provenance :carto-scan-id])))))

(deftest a-throwing-scan-id-port-does-not-fail-the-derivation
  (let [res (p/derive-model (carto/carto-source (one-cluster)
                                                (ports (fn [_] (throw (ex-info "carto down" {})))))
                            "hive-overarch" {:min-size 1})]
    (is (r/ok? res))
    (is (nil? (get-in res [:ok :provenance :carto-scan-id])))))

;; ---- model/stale? ----------------------------------------------------------

(defn- prov [scan-id]
  (m/->Provenance "c4-1" "hive-overarch" "2026-10-20T00:00:00Z" scan-id :carto :ns-prefix))

(def ^:private gen-args
  (gen/let [stored (gen/elements [nil "a" "b" ""])
            live   (gen/elements [nil "a" "b" ""])]
    [(prov stored) live]))

(deftrifecta stale-contract
  hive-overarch.model/stale?
  {:apply?      true
   :golden-path "test/golden/hive-overarch/trifecta-model-stale.edn"
   :cases       {:same-scan        [(prov "a") "a"]
                 :newer-scan       [(prov "a") "b"]
                 :no-lineage       [(prov nil) "b"]
                 :live-unknown     [(prov "a") nil]
                 :both-unknown     [(prov nil) nil]
                 :blank-live       [(prov "a") ""]
                 :no-provenance    [nil "b"]}
   :gen         gen-args
   :pred        boolean?
   :num-tests   100
   :mutations   [["always-stale" (fn [& _] true)]
                 ["never-stale" (fn [& _] false)]
                 ["unknown-live-is-stale" (fn [p live] (not= (:carto-scan-id p) live))]]})

(deftest stale-semantics
  (is (false? (m/stale? (prov "a") "a")) "same scan: current")
  (is (true? (m/stale? (prov "a") "b")) "carto re-scanned since")
  (is (true? (m/stale? (prov nil) "b")) "a snapshot with no lineage cannot vouch for a live scan")
  (is (false? (m/stale? (prov "a") nil)) "no live scan known: no evidence of staleness")
  (is (false? (m/stale? (prov "a") "  ")) "a blank live id is no scan"))
