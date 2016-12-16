(ns debug-middleware.language-server
 (:require [clojure.repl :as repl]
           [clojure.string :as str]
           [clojure.java.shell :as shell]
           [clojure.java.io :as io]
           [compliment.core])
 (:import java.io.PushbackReader
          java.io.ByteArrayOutputStream
          java.io.StringReader
          java.io.FileOutputStream
          java.io.OutputStreamWriter))

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
 (let[rdr (make-proxy (java.io.StringReader. src) (atom 0))]
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
        sig (str "```\n" sig "\n```")
        desc (str "```\n" desc "\n```")]
    [sym sig desc])
  doc-string))

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
        (catch Throwable e
          (println (.getMessage e))
          (println (.stackTrace e)))))))     

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
          ; (println "decompressing" jar-path "to" decompressed-path)
          (.mkdirs decompressed-path-dir)
          (clojure.java.shell/sh "unzip" jar-path "-d" decompressed-path))
        ; (println "DECOMPRESSED FILE PATH: " decompressed-file-path)
        decompressed-file-path))
    file-path))
  

(defn find-definition
 "Find the location where the given symbol is defined."
 [ns-str symbol-str]
 (try
  ;; Binding a thread-local copy of *ns* so changes
  ;; are isolated to this thread.
  ;; See https://groups.google.com/forum/#!msg/clojure/MGOwtVSXLS4/Jiet-nSAKzwJ)
  (binding [*ns* *ns* *e *e]
    (in-ns (symbol ns-str))
  ;;(clojure.core/require [clojure.core :refer :all])
    (require 'clojure.repl)
    (require 'clojure.java.shell)
    (require 'clojure.java.io)
    (let [sym (symbol symbol-str)
          the-var (or (some->> (or (get (ns-aliases *ns*) sym) (find-ns sym))
                              clojure.repl/dir-fn
                              first
                              name
                              (str (name sym) "/")
                              symbol)
                      sym)
          {:keys [file line]} (meta (eval `(var ~the-var)))
          file-path (.getPath (.getResource (clojure.lang.RT/baseLoader) file))]
      (if-let [[_ jar-path partial-jar-path within-file-path] (re-find #"file:(.+/\.m2/repository/(.+\.jar))!/(.+)" file-path)]
        (let [decompressed-path (str (System/getProperty "user.home")
                                    "/.lein/tmp-vscode-jars/"
                                    partial-jar-path)
              decompressed-file-path (str decompressed-path "/" within-file-path)
              decompressed-path-dir (clojure.java.io/file decompressed-path)]
          (when-not (.exists decompressed-path-dir)
              (println "decompressing" jar-path "to" decompressed-path)
              (.mkdirs decompressed-path-dir)
              (clojure.java.shell/sh "unzip" jar-path "-d" decompressed-path))
          [decompressed-file-path line])
        [file-path line])))
  (catch Exception e
    (println (.getMessage e))
    (println (.stackTrace e)))))
      
(defn refresh
 "Refresh namespaces that have changed and restart application" 
 []
 (alter-var-root #'*compiler-options* assoc :disable-locals-clearing true)
 (binding [*ns* *ns* *e *e]
  (try
    (println "Requiring 'user")
    (require 'user)
    (println "'user required.")
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
     (println "Got result.")
     (when (isa? (type result) Exception)
       (println (.getMessage result)))
     result)
   (catch Exception e
    (println "Error resolving...")
    (println (.getMessage e))
    (.printStackTrace e)))))
    
(defn run-all-tests
 "Runs all tests in the project."
 []
 (let [out-stream (ByteArrayOutputStream.)
       my-out (io/writer out-stream)
       test-out-stream (ByteArrayOutputStream.)
       my-test-out (io/writer test-out-stream)]
    (binding [clojure.test/*test-out* my-test-out
              *out* my-out]
      (println "TEST")          
      (time (clojure.test/run-all-tests)))
    {:out (.toString out-stream)
     :test-out (.toString test-out-stream)}))

(defn run-tests-in-namespace
 "Runs all the tests in a single namespace."
 [ns-str]
 (binding [*ns* *ns* *e *e]
    (in-ns (symbol ns-str))
    (clojure.test/run-tests)))

(defn run-test
 "Run a single test."
 [ns-str test-name]
 (let [the-test `(var ~(symbol (str ns-str "/" test-name)))]
    (clojure.test/test-vars [(eval the-test)])))

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