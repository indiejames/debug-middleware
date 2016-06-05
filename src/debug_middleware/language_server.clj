(ns debug-middleware.language-server
 (:require [clojure.repl :as repl]
           [clojure.java.shell :as shell]
           [clojure.java.io :as io])
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
    
(defn find-definition
 "Find the location where the given symbol is defined."
 [symbol-str]
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
          
  