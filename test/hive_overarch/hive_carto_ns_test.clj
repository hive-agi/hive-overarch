(ns hive-overarch.hive-carto-ns-test
  "The carto boundary names hive-carto namespaces, and they resolve.

   The artisan carto namespaces moved from hive-knowledge to hive-carto; the
   boundary kept the old symbols, so every carto read answered
   :capability/unavailable. The trifecta pins which namespace each capability
   is resolved from. The resolution test proves the symbols exist whenever
   hive-carto is on the classpath (run with hive-carto added, e.g.
   -Sdeps '{:deps {io.github.hive-agi/hive-carto {:local/root \"../hive-carto\"}}}');
   without it the test only checks the boundary degrades to an error."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-dsl.result :as r]
            [hive-overarch.hive :as hive]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn capability-ns
  "The namespace the boundary resolves capability K from, as a string."
  [k]
  (some-> (get hive/carto-symbols k) namespace))

(deftrifecta carto-capability-ns
  hive-overarch.hive-carto-ns-test/capability-ns
  {:golden-path "test/golden/hive-overarch/trifecta-carto-capability-ns.edn"
   :cases {:carto-search   :carto-search
           :carto-callees  :carto-callees
           :make-clusterer :make-clusterer
           :cluster        :cluster
           :unknown        :no-such-capability}
   :gen (gen/elements [:carto-search :carto-callees :make-clusterer :cluster])
   :pred string?
   :mutations [["old-hive-knowledge-ns" (fn [_] "hive-knowledge.artisan.carto.read")]
               ["nil" (constantly nil)]]})

(def ^:private carto-on-classpath?
  (hive/available? 'hive-carto.artisan.carto.read/carto-search))

(deftest every-carto-symbol-resolves-when-hive-carto-is-present
  (if carto-on-classpath?
    (do
      (doseq [[k sym] hive/carto-symbols]
        (is (hive/available? sym) (str k " -> " sym " does not resolve")))
      (doseq [ns- hive/clusterer-namespaces]
        (is (nil? (require ns-)) (str ns-)))
      (testing "the clusterer types register and construct"
        (doseq [t [:connected-components :greedy-cohesion :ns-prefix]]
          (is (r/ok? (hive/make-clusterer t)) (str t)))))
    (testing "without hive-carto the boundary answers an error, not a throw"
      (is (= :capability/unavailable (:error (hive/carto-qns "x")))))))
