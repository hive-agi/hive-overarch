(ns user
  "REPL conveniences for hive-overarch development."
  (:require [hive-overarch.model :as m]
            [hive-overarch.view :as view]
            [hive-overarch.render.overarch :as render]
            [hive-overarch.protocols :as p]))

(defn demo-view []
  (let [a  (m/element :c4/auth :system "Auth" :tech "Clojure")
        b  (m/element :c4/db   :system "DB"   :tech "Datalevin")
        r1 (m/relation :c4-rel/r0 :c4/auth :c4/db :name "reads")]
    (m/->C4View :system-landscape-view nil "Demo" [a b] [r1])))

(defn demo-render []
  (p/render-view (render/renderer) (demo-view) :plantuml))

(comment
  ;; Pure path (no hive jars needed):
  (println (:ok (demo-render)))

  ;; Live path (run with -A:with-hive inside a hive JVM):
  (require '[hive-overarch.orchestrator :as orch])
  (def sys (orch/make-system))
  (orch/snapshot! sys "hive-overarch" {})
  (orch/render!   sys "hive-overarch" {:view-kind :system-landscape-view})
  (orch/persona!  sys "hive-overarch" :c4/some-module))
