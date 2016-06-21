(ns debug-middleware.language-server
 (:require [clojure.repl :as repl]
           [clojure.java.shell :as shell]
           [clojure.java.io :as io]
           [compliment.core])
 (:import java.io.PushbackReader
          java.io.StringReader))

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

(defn doc
 "Returns the docstring for the given var."
 [var]
 (println "Finding doc for " var)
 (repl/doc var)
 (with-out-str (repl/doc var)))

(defn doc 
 "Find documentation for the given var"
  [ns-str var-str]
  (try
        ;; Binding a thread-local copy of *ns* so changes
        ;; are isolated to this thread (
        ;; see https://groups.google.com/forum/#!msg/clojure/MGOwtVSXLS4/Jiet-nSAKzwJ)
   (binding [*ns* *ns*]
     (in-ns (symbol ns-str))
     (println "In namespace " ns-str)
        ;;(clojure.core/require [clojure.core :refer :all])
     (require 'clojure.repl)
     (let [sym (symbol var-str)
           _ (println sym)
           the-var (or (some->> (or (get (ns-aliases *ns*) sym) (find-ns sym))
                               clojure.repl/dir-fn
                               first
                               name
                               (str (name sym) "/")
                               symbol)
                       sym)]
      (with-out-str (eval `(repl/doc ~the-var)))))
   (catch Exception e
     (println (.getMessage e))
     (println (.stackTrace e)))))
      
(defn find-definition
 "Find the location where the given symbol is defined."
 [ns-str symbol-str]
 (println "Finding location of " symbol-str " in namespace" ns-str)
 (try
  ;; Binding a thread-local copy of *ns* so changes
  ;; are isolated to this thread (
  ;; see https://groups.google.com/forum/#!msg/clojure/MGOwtVSXLS4/Jiet-nSAKzwJ)
  (binding [*ns* *ns*]
    (in-ns (symbol ns-str))
    (println "In namespace " ns-str)
  ;;(clojure.core/require [clojure.core :refer :all])
    (require 'clojure.repl)
    (require 'clojure.java.shell)
    (require 'clojure.java.io)
    (let [sym (symbol symbol-str)
          _ (println sym)
          the-var (or (some->> (or (get (ns-aliases *ns*) sym) (find-ns sym))
                              clojure.repl/dir-fn
                              first
                              name
                              (str (name sym) "/")
                              symbol)
                      sym)
          _ (println the-var)
          {:keys [file line]} (meta (eval `(var ~the-var)))
          _ (println file)
          _ (println line)
          file-path (.getPath (.getResource (clojure.lang.RT/baseLoader) file))]
      (if-let [[_ jar-path partial-jar-path within-file-path] (re-find #"file:(.+/\\.m2/repository/(.+\\.jar))!/(.+)" file-path)]
        (let [decompressed-path (str (System/getProperty "user.home")
                                    "/.lein/tmp-atom-jars/"
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
 (binding [*ns* *ns*]
  (try
    (println "Requiring 'user")
    (require 'user)
    (println "'user required.")
    (catch java.io.FileNotFoundException e
      (println (str "No user namespace defined. Defaulting to clojure.tools.namespace.repl/refresh.\n"))))
  (try
    (println "Requiring 'clojure.tools.namespace.repl")
    (require 'clojure.tools.namespace.repl)
    (println "'clojure.tools.namespace.repl required.")
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

(defn get-completions
 "Returns completions using Compliment"
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