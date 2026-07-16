(ns hive-overarch.derive.cluster
  "IElementClusterer adapter over hive-knowledge's carto graph clusterers
   (:greedy-cohesion / :connected-components / :ns-prefix). Strategy seam."
  (:require [hive-overarch.protocols :as p]
            [hive-overarch.hive :as hive]
            [hive-dsl.result :as r]))

(defrecord CartoClusterer [type]
  p/IElementClusterer
  (cluster-graph [_ graph opts]
    (r/let-ok [clusterer (hive/make-clusterer type)]
      (hive/cluster clusterer graph opts))))

(defn clusterer
  ([] (clusterer :greedy-cohesion))
  ([type] (->CartoClusterer type)))
