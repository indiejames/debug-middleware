(defproject debug-middleware "0.4.5"
  :description "nREPL middleware to support VS Code Continuum"
  :url "https://github.com/indiejames/debug-middleware"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-modules "0.3.11"]]
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :sign-releases false}]]
  :dependencies [[compliment "0.2.7"]
                 [cljfmt "0.5.6"]
                 [org.clojars.jnorton/cdt "1.2.6.5"]
                 [cheshire "5.6.3"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 [slamhound "1.5.5"]])
