(ns user
  (:require
   [clj-reload.core :as clj-reload]
   [clojure+.hashp :as hashp]
   [clojure+.print :as print]
   [clojure+.error :as error]
   [clojure+.test :as test]
   [duti.core :as duti]))

(hashp/install!)
(print/install!)
(error/install!)

(clj-reload/init
  {:dirs      ["src" "dev" "test"]
   :no-reload '#{user}})

(def reload
  clj-reload/reload)
