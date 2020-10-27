(defproject debug-middleware "0.5.4"
  :description "nREPL middleware to support VS Code Continuum"
  :url "https://github.com/indiejames/debug-middleware"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-modules "0.3.11"]]
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :sign-releases false}]]
  :dependencies [[cheshire "5.10.0"]
                 [cljfmt "0.7.0"]
                 [compliment "0.3.11"]
                 [eftest "0.5.9"]
                 [io.aviso/pretty "0.1.37"]
                 [mvxcvi/puget "1.3.1"]
                 [nrepl "0.8.3"]
                 [org.clojars.jnorton/cdt "1.2.6.5"]
                 [org.clojure/tools.namespace "1.0.0"]
                 [org.clojure/tools.logging "1.1.0"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "1.3.610"]
                 [pjstadig/humane-test-output "0.10.0"]
                 [slamhound "1.5.5"]])
