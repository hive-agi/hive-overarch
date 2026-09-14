(ns hive-overarch.derive-carto-test
  (:require [clojure.test :refer :all]
            [hive-overarch.derive.carto :as carto]))

(def ^:private prefix #'carto/common-ns-prefix)

(deftest unchanged-branches
  (testing "empty namespaces"
    (is (= "module" (prefix []))))
  (testing "single namespace"
    (is (= "a.b" (prefix ["a.b"]))))
  (testing "common prefix of 2+ segments"
    (is (= "hive-overarch.store" (prefix ["hive-overarch.store.kg" "hive-overarch.store.mem"]))))
  (testing "no common first segment"
    (is (= "x.y" (prefix ["x.y" "z.w"])))))

(deftest root-only-prefix-is-disambiguated
  (testing "two second segments, alphabetically first dominant"
    (is (= "hive-overarch.model+1" (prefix ["hive-overarch.model" "hive-overarch.render"]))))
  (testing "three namespaces, mixed second segments"
    (is (= "a.x+1" (prefix ["a.x.p" "a.x.q" "a.y"]))))
  (testing "root plus one namespace with second segment"
    (is (= "a.x" (prefix ["a" "a.x"])))))
