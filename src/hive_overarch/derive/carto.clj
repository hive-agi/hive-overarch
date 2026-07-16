(ns hive-overarch.derive.carto
  "IModelSource that derives a C4 model SNAPSHOT from the carto code-graph:
   enumerate qns -> build the qn dependency graph (parallel reads) -> cluster
   into modules -> emit each cluster as a C4 :system element with inter-module
   :depends-on relations. The result is an immutable, provenance-stamped value."
  (:require [clojure.string :as str]
            [hive-weave.parallel :as wp]
            [hive-overarch.protocols :as p]
            [hive-overarch.hive :as hive]
            [hive-overarch.model :as m]
            [hive-dsl.result :as r]))

(defn- qn->ns [qn]
  (first (str/split (str qn) #"/" 2)))

(defn- common-ns-prefix [namespaces]
  (let [splits (map #(str/split % #"\.") namespaces)]
    (cond
      (empty? splits)       "module"
      (= 1 (count splits))  (first namespaces)
      :else
      (let [cols   (apply map vector splits)
            common (take-while #(apply = %) cols)]
        (if (seq common)
          (str/join "." (map first common))
          (first namespaces))))))

(defn- cluster->element [idx qns]
  (let [namespaces (distinct (map qn->ns qns))
        nm         (common-ns-prefix namespaces)
        id         (keyword "c4" (str (str/replace nm #"[./]" "-") "-" idx))]
    (m/element id :system nm
               :tech "Clojure"
               :description (str (count qns) " forms across " (count namespaces) " namespace(s)")
               :carto-refs qns)))

(defn- qn-edges
  "Parallel carto reads -> set of [from-qn to-qn] depends-on pairs."
  [qns scope]
  (let [results (wp/bounded-pmap {:concurrency 8 :timeout-ms 15000 :fallback nil}
                                 (fn [qn] (hive/carto-callees qn scope))
                                 qns)]
    (into #{}
          (mapcat (fn [qn res]
                    (when (and res (r/ok? res))
                      (for [c   (:ok res)
                            :let [to (:qn c)]
                            :when to]
                        [qn to])))
                  qns results))))

(defn- cluster-relations [edges qn->eid]
  (->> edges
       (keep (fn [[from to]]
               (let [ef (qn->eid from) et (qn->eid to)]
                 (when (and ef et (not= ef et)) [ef et]))))
       distinct
       (map-indexed (fn [i [ef et]]
                      (m/relation (keyword "c4-rel" (str "r" i)) ef et :name "depends on")))))

(defrecord CartoDerivedSource [clusterer]
  p/IModelSource
  (derive-model [_ scope opts]
    (r/let-ok [qn-rows (hive/carto-qns scope)
               :let [qns   (vec (keep :qn qn-rows))
                     edges (qn-edges qns scope)
                     graph {:nodes (set qns) :edges edges}]
               clusters (p/cluster-graph clusterer graph
                                         {:min-size (:min-size opts 3)
                                          :max-size (:max-size opts 12)})]
      (let [clusters (vec clusters)
            elements (vec (map-indexed cluster->element clusters))
            qn->eid  (into {} (mapcat (fn [el qset] (map (fn [q] [q (:id el)]) qset))
                                      elements clusters))
            relations (vec (cluster-relations edges qn->eid))
            prov (m/->Provenance (str "c4-" (System/currentTimeMillis))
                                 scope (str (java.time.Instant/now)) nil
                                 :carto (:type clusterer))]
        (r/ok (m/model elements relations prov))))))

(defn carto-source
  ([clusterer] (->CartoDerivedSource clusterer)))
