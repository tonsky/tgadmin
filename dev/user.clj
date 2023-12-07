(ns user
  (:require
    [clojure.core.server :as server]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :as ns]))

(ns/disable-reload!)

(ns/set-refresh-dirs "src" "dev")

(def lock
  (Object.))

(defn position []
  (let [trace (->> (Thread/currentThread)
                (.getStackTrace)
                (seq))
        el    ^StackTraceElement (nth trace 4)]
    (str "[" (clojure.lang.Compiler/demunge (.getClassName el)) " " (.getFileName el) ":" (.getLineNumber el) "]")))

(defn p [form]
  `(let [t# (System/currentTimeMillis)
         res# ~form]
     (locking lock
       (println (str "#p" (position) " " '~form " => (" (- (System/currentTimeMillis) t#) " ms) " (pr-str res#))))
     res#))

(def *reloaded
  (atom #{}))

(add-watch #'ns/refresh-tracker ::log
  (fn [_ _ _ new]
    (swap! *reloaded into (:clojure.tools.namespace.track/load new))))

(defn reload []
  (set! *warn-on-reflection* true)
  (reset! *reloaded #{})
  (let [res (ns/refresh)]
    (when (not= :ok res)
      (.printStackTrace ^Throwable res)
      (throw res)))
  (str "Ready – " (count @*reloaded) " ns" (when (> (count @*reloaded) 1) "es")))

(defn -main [& args]
  (alter-var-root #'*command-line-args* (constantly args))
  (require 'tgadmin.core)
  (reload)
  (let [args (apply array-map args)
        port (or
               (some-> (get args "--repl-port") parse-long)
               (+ 1024 (rand-int 64512)))
        file (io/file ".repl-port")]
    (server/start-server
      {:name          "repl"
       :port          port
       :accept        'clojure.core.server/repl
       :server-daemon false})
    (println "Started Socket REPL server on port" port)
    (spit file port)
    (.deleteOnExit file)))

