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
   :output    :quieter
   :no-reload '#{user}})

(def reload
  clj-reload/reload)

(defn -main [& args]
  (let [args (apply array-map args)
        ;; starting app
        _    (set! *warn-on-reflection* true)
        _    (require 'tgadmin.core)
        ;; starting socket repl
        ; port (some-> (get args "--port") parse-long)
        ; _    (duti/start-socket-repl {:port port})
        ]))
