(ns hive-overarch.view
  "Pure projection of a C4Model down to a single C4View. No IO."
  (:require [hive-overarch.protocols :as p]
            [hive-overarch.model :as m]
            [hive-dsl.result :as r]))

(def ^:private c4-view-kinds
  #{:context-view :container-view :component-view
    :system-landscape-view :deployment-view :dynamic-view})

(defn- whole-model-view [model kind]
  (m/->C4View kind nil (name kind) (:elements model) (:relations model)))

(defn- focused-view [model kind focus-id]
  (let [{:keys [elements relations]} (m/subgraph model focus-id)]
    (m/->C4View kind focus-id (str (name kind) " @ " focus-id) elements relations)))

(defrecord DefaultViewProjection []
  p/IViewProjection
  (project [_ model kind focus-id]
    (cond
      (not (contains? c4-view-kinds kind))
      (r/err :view/unknown-kind {:kind kind :known c4-view-kinds})

      (nil? focus-id)
      (r/ok (whole-model-view model kind))

      (nil? ((m/index-by-id model) focus-id))
      (r/err :view/unknown-focus {:focus focus-id})

      :else
      (r/ok (focused-view model kind focus-id)))))

(defn default-projection [] (->DefaultViewProjection))
