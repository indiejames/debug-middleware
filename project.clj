(defproject debug-middleware "0.5.1"
  :description "nREPL middleware to support VS Code Continuum"
  :url "https://github.com/indiejames/debug-middleware"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-modules "0.3.11"]]
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :sign-releases false}]]
  :dependencies [[cheshire "5.6.3"]
                 [cljfmt "0.5.6"]
                 [compliment "0.2.7"]
                 [eftest "0.3.0"]
                 [io.aviso/pretty "0.1.33"]
                 [mvxcvi/puget "1.0.1"]
                 [org.clojars.jnorton/cdt "1.2.6.5"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 [slamhound "1.5.5"]])
