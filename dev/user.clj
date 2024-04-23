(ns user
  (:require
    [clj-reload.core :as reload]
    [duti.core :as duti]))

(reload/init
  {:dirs ["src"]})

(defn reload [& [opts]]
  (set! *warn-on-reflection* true)
  (let [res (reload/reload opts)
        cnt (count (:loaded res))]
    (str "Reloaded " cnt " namespace" (when (not= 1 cnt) "s"))))

(defn -main [& args]
  (let [args (apply array-map args)
        ;; starting app
        _    (set! *warn-on-reflection* true)
        _    (require 'tgadmin.core)
        ;; starting socket repl
        port (some-> (get args "--port") parse-long)
        _    (duti/start-socket-repl {:port port})]))
