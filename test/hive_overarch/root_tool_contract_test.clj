(ns hive-overarch.root-tool-contract-test
  (:require [hive-addon.tool-contract.test :refer [deftest-root-tools]]
            [hive-addon.protocol :as proto]
            [hive-overarch.init :as a]))

;; hive-overarch exposes no standalone root tools (tools [] by design).
(deftest-root-tools root-tools (proto/tools (a/addon-ctor {})) :allow-empty? true)
