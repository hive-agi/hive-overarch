(ns hive-overarch.model
  "Immutable C4 value types and pure graph helpers. A C4Model is a SNAPSHOT:
   a value taken at a point in time, carrying its own Provenance. It is never
   mutated in place — a new state of the world is a new snapshot."
  (:require [clojure.set :as set]))

;; kind for C4Element matches Overarch :el node keywords:
;;   :person :system :container :component :node
;; kind for C4Relation matches Overarch :el relation keywords (default :rel).
(defrecord Provenance    [snapshot-id scope taken-at carto-scan-id source clusterer])
(defrecord C4Element     [id kind name tech description external tags carto-refs])
(defrecord C4Relation    [id kind from to name tech])
(defrecord C4Model       [elements relations provenance])
(defrecord C4View        [kind focus title elements relations])
(defrecord PersonaContext [element-id prompt-fragment ctx-refs kg-node-ids scope])
(defrecord SnapshotRef   [id scope taken-at element-count])

(defn element
  [id kind name & {:keys [tech description external tags carto-refs]}]
  (->C4Element id kind name tech description (boolean external)
               (set tags) (set carto-refs)))

(defn relation
  [id from to & {:keys [kind name tech]}]
  (->C4Relation id (or kind :rel) from to name tech))

(defn model
  [elements relations provenance]
  (->C4Model (vec elements) (vec relations) provenance))

;; ---- pure graph helpers (used by view projection + persona) ----

(defn index-by-id
  "Map of element-id -> C4Element."
  [{:keys [elements]}]
  (into {} (map (juxt :id identity)) elements))

(defn out-relations
  "Relations whose :from is id."
  [{:keys [relations]} id]
  (filter #(= id (:from %)) relations))

(defn in-relations
  "Relations whose :to is id."
  [{:keys [relations]} id]
  (filter #(= id (:to %)) relations))

(defn neighbor-ids
  "Ids directly connected to id in either direction."
  [m id]
  (set/union (set (map :to (out-relations m id)))
             (set (map :from (in-relations m id)))))

(defn subgraph
  "The element at id plus its immediate neighbors and the relations among them."
  [m id]
  (let [idx        (index-by-id m)
        ns-ids     (conj (neighbor-ids m id) id)
        elems      (keep idx ns-ids)
        rels       (filter (fn [{:keys [from to]}]
                             (and (contains? ns-ids from) (contains? ns-ids to)))
                           (:relations m))]
    {:focus (idx id) :elements (vec elems) :relations (vec rels)}))
