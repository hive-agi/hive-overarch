(ns hive-overarch.ecosystem.probe
  "Collect layer: probe a monorepo root for per-project facts (deps.edn
   coordinates, git remote, IAddon manifest). Filesystem access goes through
   hive-system (IPathQuery via fs.core); read-text is the one primitive
   hive-system lacks, owned here as an injectable port. Parsing is pure."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [malli.core :as mc]
            [hive-dsl.result :as r]
            [hive-system.fs.core :as fs]
            [hive-overarch.ecosystem.schema :as s]))

;; ---- pure parsing ----

(defn parse-remote-url
  "First `url = ...` in a .git/config text, or nil."
  [config-text]
  (second (re-find #"(?m)^\s*url\s*=\s*(\S+)" (or config-text ""))))

(mc/=> parse-remote-url [:=> [:cat [:maybe :string]] [:maybe :string]])

(defn- dep-map-keys [deps-map]
  (concat (keys (:deps deps-map))
          (mapcat (fn [alias] (concat (keys (:extra-deps alias))
                                      (keys (:replace-deps alias))
                                      (keys (:deps alias))))
                  (vals (:aliases deps-map)))))

(defn sibling-dep-names
  "Names of coordinates under any of the given orgs (dep groupIds) anywhere
   in a deps.edn map."
  [orgs deps-map]
  (into #{}
        (keep #(when (contains? orgs (namespace %)) (name %)))
        (filter qualified-symbol? (dep-map-keys deps-map))))

(mc/=> sibling-dep-names [:=> [:cat [:set :string] [:maybe :map]] [:set :string]])

;; ---- fs seam (hive-system-backed defaults) ----

(defn- basename [path]
  (last (str/split (str path) #"/")))

(def default-fs
  "hive-system-backed probe seam. :list-dirs / :list-files -> seq of paths
   (nil on error); :read-file -> file text or nil (missing-primitive port,
   owned here until hive-system grows a read protocol)."
  {:list-dirs  (fn [root]
                 (let [res (fs/children root {:filter :dirs})]
                   (when (r/ok? res) (:ok res))))
   :list-files (fn [dir]
                 (let [res (fs/children dir {:filter :files})]
                   (when (r/ok? res) (:ok res))))
   :read-file  (fn [path]
                 (let [res (fs/file? path)]
                   (when (and (r/ok? res) (:ok res))
                     (try (slurp path) (catch Exception _ nil)))))})

;; ---- collection ----

(defn- read-edn [text]
  (when text
    (try (edn/read-string text) (catch Exception _ nil))))

(defn- project-fact [fs orgs dir]
  (let [pname     (basename dir)
        deps-text ((:read-file fs) (str dir "/deps.edn"))]
    (when deps-text
      (let [manifest-paths ((:list-files fs) (str dir "/resources/META-INF/hive-addons"))
            manifest       (some-> (first (sort manifest-paths))
                                   ((:read-file fs))
                                   read-edn)]
        {:name           pname
         :dir            (str dir)
         :deps           (disj (sibling-dep-names orgs (read-edn deps-text)) pname)
         :remote         (parse-remote-url ((:read-file fs) (str dir "/.git/config")))
         :addon?         (boolean (seq manifest-paths))
         :addon-manifest manifest}))))

(defn collect-facts
  "Monorepo root -> Result<EcosystemFacts>. Sibling deps are matched against
   the EcoConfig :dep-orgs groupIds. A directory without deps.edn is skipped;
   a project whose deps.edn fails to parse keeps an empty :deps set."
  ([root] (collect-facts root {}))
  ([root {:keys [fs config] :or {fs default-fs}}]
   (r/try-effect* :ecosystem/collect-failed
     {:root     (str root)
      :projects (vec (keep #(project-fact fs (or (:dep-orgs config) #{}) %)
                           (sort (map str ((:list-dirs fs) root)))))})))
