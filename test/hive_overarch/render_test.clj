(ns hive-overarch.render-test
  "Real integration test against the upstream Overarch renderer (no hive deps)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-overarch.model :as m]
            [hive-overarch.view :as view]
            [hive-overarch.render.overarch :as render]
            [hive-overarch.protocols :as p]
            [hive-dsl.result :as r]))

(defn- landscape-view []
  (let [a  (m/element :c4/auth :system "Auth" :tech "Clojure")
        b  (m/element :c4/db   :system "DB"   :tech "Datalevin")
        r1 (m/relation :c4-rel/r0 :c4/auth :c4/db :name "reads")]
    (m/->C4View :system-landscape-view nil "Landscape" [a b] [r1])))

(deftest render-plantuml-smoke
  (let [out (p/render-view (render/renderer) (landscape-view) :plantuml)]
    (testing "renders an ok Result"
      (is (r/ok? out) (str "render failed: " (pr-str out))))
    (testing "produces a PlantUML document"
      (is (str/includes? (:ok out) "@startuml"))
      (is (str/includes? (:ok out) "@enduml")))))

(deftest unsupported-format-is-error
  (let [out (p/render-view (render/renderer) (landscape-view) :svg)]
    (is (r/err? out))
    (is (= :render/unsupported-format (:error out)))))

(deftest view-projection-roundtrip
  (let [prov  (m/->Provenance "c4-1" "demo" "t" nil :carto :greedy-cohesion)
        model (m/model [(m/element :c4/a :system "A")
                        (m/element :c4/b :system "B")]
                       [(m/relation :c4-rel/r0 :c4/a :c4/b :name "uses")]
                       prov)
        projected (p/project (view/default-projection) model :system-landscape-view nil)]
    (is (r/ok? projected))
    (is (= 2 (count (:elements (:ok projected)))))))
