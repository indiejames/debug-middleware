(ns debug-middleware.language-server
 (:require [cljfmt.core :as clj-fmt]
           [clojure.java.io :as io]
           [clojure.repl :as repl]
           [clojure.string :as str]
           [clojure.java.shell :as shell]
           [debug-middleware.test :as debug-test]
           [eftest.report.pretty :as pretty]
           [eftest.runner :refer [find-tests run-tests]]
           [io.aviso.ansi :as ansi]
           [slam.hound :refer [reconstruct]]
           [compliment.core])
 (:import java.io.PushbackReader
          java.io.ByteArrayOutputStream
          java.io.StringReader
          java.io.File
          java.io.FileOutputStream
          java.io.OutputStreamWriter
          java.lang.management.ManagementFactory
          java.util.jar.JarFile))

(defn make-proxy
 "Returns a proxy for a PushbackReader that returns a count of the number of characters
 read so far."
 [reader count-atom]
 (proxy [java.io.PushbackReader clojure.lang.IDeref] [reader]
  (deref [] @count-atom)
  (read [] (do (swap! count-atom inc) (proxy-super read)))
  (unread [c] (do (swap! count-atom dec) (proxy-super unread c)))))

(defn find-high-level-form
 [src position]
 (let [rdr (make-proxy (java.io.StringReader. src) (atom 0))]
   (loop [form (read rdr) pos @rdr]
    (if (> pos position)
     form
     (recur (read rdr) @rdr)))))

(defn format-doc
 "Format a docstring using markdown."
 [doc-string]
 (if (seq doc-string)
  (let [[_ sym sig desc] (re-matches #"(?s).*?\n(.*?)\n(.*?)\n(.*)" doc-string)
        ; sym (str "[" sym "]()")
        sym (str "**" sym "**")
        sig (str "```clojure\n" sig "\n```")]
    [sym sig desc])
  doc-string))

(defn fix-ns
  "Fix requires/imports for the ns entry for the given source file."
  [path]
  (reconstruct path))
  
(defn get-doc
 "Find documentation for the given var"
  [ns-str var-str]
  ;; Binding a thread-local copy of *ns* so changes
  ;; are isolated to this thread (
  ;; see https://groups.google.com/forum/#!msg/clojure/MGOwtVSXLS4/Jiet-nSAKzwJ)
  (let [name-space (find-ns (symbol ns-str))
        sym (symbol var-str)]
    (binding [*ns* name-space *e *e]
      (try
        (require 'clojure.repl)
        (let [the-var (or (some->> (or (get (ns-aliases *ns*) sym) (find-ns sym))
                               ns-name)
                          sym)
              rval (or (with-out-str (eval `(clojure.repl/doc ~the-var))) "NO VALUE")
              rval (format-doc rval)
              rval (if (= rval "") "NO VALUE" rval)]
          rval)
        (catch Throwable e)))))
          ;; Output here shows up in the REPL and is confusing in most cases,
          ;; So I have eliminated it here. Leavning this in place in case
          ;; I change my mind.
          ;; (binding [*out* *err*]
          ;;   (println (.getMessage e))))))))

(defn get-args
  "Find the arguments for the given function."
  [ns-str var-str]
  (let [name-space (find-ns (symbol ns-str))
        sym (symbol var-str)]
    (binding [*ns* name-space *e *e]
      (try
        (let [the-var (or (some->> (or (get (ns-aliases *ns*) sym) (find-ns sym))
                               ns-name)
                          sym)
              ev (eval `(var ~the-var))
              mta (meta ev)
              arg-lists (:arglists mta)
              rval (when arg-lists (first arg-lists))]
          rval)
        (catch Throwable e)))))

(defn get-signatures
  "Find the signatures for the given function."
  [ns-str var-str]
  (let [name-space (find-ns (symbol ns-str))
        sym (symbol var-str)]
    (binding [*ns* name-space *e *e]
      (try
        (let [the-var (or (some->> (or (get (ns-aliases *ns*) sym) (find-ns sym))
                               ns-name)
                          sym)
              ev (eval `(var ~the-var))
              mta (meta ev)
              arg-lists (:arglists mta)]
          arg-lists)
        (catch Throwable e)))))

(defn get-src-path
  "Returns the readable source path for the given internal source path. Will
  expand jar files as necessary to make the file readable by the editor."
  [file]
  (let [file-path (.getPath (.getResource (clojure.lang.RT/baseLoader) file))
        jar-regex #"file:(.+/\.m2/repository/(.+\.jar))!/(.+)"]
    (if-let [[_ jar-path partial-jar-path within-file-path] (re-find jar-regex file-path)]
      (let [decompressed-path (str (System/getProperty "user.home")
                                   "/.lein/tmp-vscode-jars/"
                                   partial-jar-path)
            decompressed-file-path (str decompressed-path "/" within-file-path)
            decompressed-path-dir (clojure.java.io/file decompressed-path)]
        (when-not (.exists decompressed-path-dir)
          (.mkdirs decompressed-path-dir)
          (clojure.java.shell/sh "unzip" jar-path "-d" decompressed-path))
        decompressed-file-path)
      file-path)))

(defn find-definition
  "Find the location where the given symbol is defined."
  [ns-str symbol-str]
  (try
    (binding [*ns* *ns* *e *e]
      (in-ns (symbol ns-str))
      (require 'clojure.repl)
      (require 'clojure.java.shell)
      (require 'clojure.java.io)
      (let [var-sym (symbol symbol-str)
            the-var (or (some->> (or (get (ns-aliases *ns*) var-sym) (find-ns var-sym))
                                 clojure.repl/dir-fn
                                 first
                                 name
                                 (str (name var-sym) "/")
                                 symbol)
                        var-sym)
            ev (eval `(var ~the-var))
            mta (meta ev)
            {:keys [file line protocol]} mta
            file-path (when file (loop [paths (remove empty? (clojure.string/split (.getPath (.toURI (java.io.File. file))) #"/"))]
                                  (when-not (empty? paths)
                                    (let [path (clojure.string/join "/" paths)
                                          res (.getResource (clojure.lang.RT/baseLoader) path)]
                                      (if-not (nil? res)
                                              (let [uri (.normalize (.toURI (.getResource (clojure.lang.RT/baseLoader) path)))]
                                                (if (.isOpaque uri)
                                                  (let [url (.toURL uri)
                                                        conn (.openConnection url)
                                                        file (java.io.File. (.. conn getJarFileURL toURI))]
                                                    (str (.getAbsolutePath file) "!" (second (clojure.string/split (.getPath url) #"!"))))
                                                  (.getAbsolutePath (java.io.File. uri))))
                                              (recur (rest paths)))))))]
        (if protocol
          {:error "Definition lookup for protocol methods is not supported."}
          (if-let [[_
                    jar-path
                    partial-jar-path
                    within-file-path] (re-find #"(.+\.m2.repository.(.+\.jar))!/(.+)" file-path)]
            (let [decompressed-path (.getAbsolutePath
                                      (File. (.getAbsolutePath (File.
                                                                (System/getProperty "user.home")
                                                                "/.lein/tmp-vscode-jars/"))
                                            partial-jar-path))
                  decompressed-file-path (.getAbsolutePath (File. decompressed-path within-file-path))
                  decompressed-path-dir (clojure.java.io/file decompressed-path)]
              (when-not (.exists decompressed-path-dir)
                (println "decompressing" jar-path "to" decompressed-path)
                (.mkdirs decompressed-path-dir)
                (let [jar-file (JarFile. jar-path)]
                  (run! (fn [jar-entry]
                          (let [file (File. decompressed-path (.getName jar-entry))]
                            (if (.isDirectory jar-entry)
                              (.mkdir file)
                              (with-open [is (.getInputStream jar-file jar-entry)]
                                (clojure.java.io/copy is file)))))
                        (seq (.toArray (.stream jar-file))))))
              {:path decompressed-file-path :line line})
            {:path file-path :line line}))))
    (catch Exception e
      ;; (binding [*out* *err*])
        ;; (println (.getMessage e)))
      {:error (str "Definition not available for symbol " symbol-str)})))

(defn load-source-file
  "Load the clojure source file at the given path."
  [file-path]
  (println "Loading file path " file-path)
  (load-file file-path))

(defn refresh
 "Refresh namespaces that have changed and restart application"
 []
 (alter-var-root #'*compiler-options* assoc :disable-locals-clearing true)
 (binding [*ns* *ns* *e *e]
  (println "Refreshing code")
  (try
    (println "Requiring 'user")
    (require 'user)
    (catch java.io.FileNotFoundException e
      (println (str "No user namespace defined. Defaulting to clojure.tools.namespace.repl/refresh.\n"))))
  (try
    (println "Requiring clojure.tools.namespace.repl")
    (require 'clojure.tools.namespace.repl)
    (println "clojure.tools.namespace.repl required.")
    (catch java.io.FileNotFoundException e
      (println "clojure.tools.namespace.repl not available. Add as a dependency to allow refresh.")))
  (try
   (let [user-reset 'user/reset
         ctnr-refresh 'clojure.tools.namespace.repl/refresh
         result (cond
                   (find-var user-reset)
                   ((resolve user-reset))
                   (find-var ctnr-refresh)
                   ((resolve ctnr-refresh))
                   :else
                   (println (str "You can use your own refresh function, just define reset function in user namespace\n"
                                 "See this https://github.com/clojure/tools.namespace#reloading-code-motivation for why you should use it")))]
     (when (isa? (type result) Exception)
       (println (.getMessage result)))
     (println "Refresh completed")
     result)
   (catch Exception e
    (binding [*out* *err*]
      (println (.getMessage e))
      (.printStackTrace e))))))

(defn super-refresh
  "Force reload all namespaces whether they have changed or not."
  []
  (when (find-ns 'clojure.tools.namespace.repl)
    (eval '(clojure.tools.namespace.repl/clear)))
  (refresh))

(defn- combine-test-reports
  "Combine the results from multiple test runs into a single report"
  [& reports]
  (println reports)
  (let [reports (filter seq reports)
        reports (map #(dissoc % :type) reports)]
    (apply merge-with + reports)))

(defn- pluralize-failures
  "Returns \"failure\" for one failure,
  \"failures\" for zero or man failures."
  [count]
  (if (= count 1)
    "failure"
    "failures"))

(defn- pluralize-errors
  "Returns \"error\" for one error,
  \"errors\" for zero or many errors."
  [count]
  (if (= count 1)
    "error"
    "errors"))

(defn run-tests-in-dirs
  "Run tests in the given directories. The multithread? (default false)
  flag determines whether or not the tests are run in parallel or one at 
  a time."
  ([dirs] (run-tests-in-dirs dirs false))
  ([dirs multithread?]
   (when (seq dirs)
     (run-tests (find-tests dirs) {:multithread? multithread?}))))

(defn- fix-error-paths
  "Fix the paths in error reports. Often these link to the place
  where an exception occurred, not to the test that caused it. This
  function changes these to start of the test."
  [report-data]
  (update-in 
   report-data 
   [:error]
   (fn [errors]
     (map (fn [error]
            (let [{:keys [source]} error
                  source (ansi/strip-ansi source)
                  matcher (re-matcher #"(.*?)/(.*?) \((.*?):(\d+)\)" source)
                  [_ test-namespace test-str rep-file rep-line] (re-find matcher)
                  {:keys [path line]} (find-definition test-namespace test-str)]
              (if (str/ends-with? path rep-file)
               ;; reported file matches test file
               (-> error
                   (update-in [:source] ansi/strip-ansi))
               ;; reported file does not match test file, so use test file
               (-> error 
                   (assoc :source path)
                   (assoc :line line)))))
          errors))))
               
(defn run-all-tests
 "Runs all tests in the project."
 [parallel-dirs sequential-dirs]
 (debug-test/reset-report-data!)
 (println "Runnning all tests ............................................................")
 (let [_ (println "Running parallel tests")
       par-report (run-tests-in-dirs parallel-dirs true)
       _ (println "Running sequential tests" sequential-dirs)
       seq-report (run-tests-in-dirs sequential-dirs)
       merged-report (combine-test-reports par-report seq-report)
       {:keys [test pass fail error duration]} merged-report
       color (if (= 0 fail error)
                 (:pass pretty/*fonts*)
                 (:fail pretty/*fonts*))]
    (println (format "Ran %d test(s) in %.3f seconds" test (/ duration 1000.0)))
    (println (str color (format "%d %s, %d %s"
                                fail 
                                (pluralize-failures fail) 
                                error 
                                (pluralize-errors error))))
   (let [report-data @debug-test/report-data
         rval (fix-error-paths @debug-test/report-data)
         x (assoc rval :x 1)]
     rval)))

(defn run-tests-in-namespace
 "Runs all the tests in a single namespace."
 [ns-str]
 (debug-test/reset-report-data!)
 (println "Running tests in namespace [" ns-str "] ..................................")
 (require (symbol ns-str))
 (run-tests (find-tests (symbol ns-str)) {:multithread? false})
 (let [report-data @debug-test/report-data
         rval (fix-error-paths @debug-test/report-data)
         x (assoc rval :x 1)]
     rval))

(defn run-test
 "Run a single test."
 [ns-str test-name]
 (debug-test/reset-report-data!)
 (require (symbol ns-str))
 (let [the-test `(var ~(symbol (str ns-str "/" test-name)))]
    (run-tests (find-tests (eval the-test))))
 (let [report-data @debug-test/report-data
         rval (fix-error-paths @debug-test/report-data)
         x (assoc rval :x 1)]
     rval))
    
(comment
  (run-all-tests ["test"] [])
  (run-test "debug-middleware.core-test" "a-test")
  (run-tests-in-namespace "debug-middleware.core-test")
  (require 'debug-middleware.core-test)
  (run-tests (find-tests (symbol "debug-middleware.core-test"))))

(defn get-completions
 "Returns completions using Compliment."
  [namespace prefix src pos]
  (let [ns-symbol (symbol namespace)
        ctx (str (find-high-level-form src pos))
        completions (compliment.core/completions
                       prefix
                       {:tag-candidates true
                        :ns ns-symbol
                        :ctx ctx})]
    (->> completions
        (take 100)
        (mapv #(assoc % :docs (compliment.core/documentation
                                (:candidate %) ns-symbol))))))

(defn pid
  "Get JVM process PID"
  []
  (-> (ManagementFactory/getRuntimeMXBean)
      .getName
      (str/split #"@")
      first
      Integer/parseInt))

;; reformat the string containing Clojure code
(defn reformat-string
  [code opts]
  (clj-fmt/reformat-string code opts))
