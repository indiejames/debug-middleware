(ns debug-middleware.test
  "Provides enhanced test output."
  (:require
   [clojure.string :as str] 
   [clojure.test :as t]
   [eftest.report.pretty :as pretty :refer [report]]
   [eftest.report :as r]
   [eftest.runner :refer [find-tests run-tests]]
   [io.aviso.ansi :as ansi]
   [io.aviso.exception :as exception]
   [io.aviso.repl :as repl]
   [puget.printer :as puget]))


(def cprint-options
  "Options to use when color pretty printing with puget"
  {:width 80
   :sort-keys 80
   :map-delimiter ","
   :map-coll-separator " "
   :print-fallback :pretty
   :print-color true
   :color-markup :ansi
   :color-scheme
   {; syntax elements
    :delimiter [:white]
    :tag       [:red]

    ; primitive values
    :nil       [:white]
    :boolean   [:cyan]
    :number    [:cyan]
    :string    [:green]
    :character [:magenta]
    :keyword   nil
    :symbol    nil

    ; special types
    :function-symbol [:blue]
    :class-delimiter [:magenta]
    :class-name      [:magenta]}})


(defmethod report :fail
  [m]
  (let [fail-count (:fail @t/*report-counters*)]
    (t/with-test-out
      (t/inc-report-counter :fail)
      (println (str "# FAIL-START " fail-count " #############################################"))
      (println "\nFAIL in" (t/testing-vars-str m))
      (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
      (when-let [message (:message m)] (println message))
      (println "expected:" (puget/cprint-str (:expected m) cprint-options))
      (println "  actual:" (puget/cprint-str (:actual m) cprint-options))
      (println (str "# FAIL-END " fail-count " ###############################################")))))

(defmethod report :error 
  [{:keys [message expected actual] :as m}]
  (let [error-count (:error @t/*report-counters*)]
    (t/with-test-out
      (t/inc-report-counter :error)
      (println (str "# ERROR-START " error-count " #############################################"))
      (println (str (:error pretty/*fonts*) "ERROR" (:reset pretty/*fonts*) " in") (t/testing-vars-str m))
      (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
      (when message (println message))
      (println "expected:" (puget/cprint-str expected cprint-options))
      (print "  actual: ")
      (if (instance? Throwable actual)
        (binding [exception/*traditional* true, exception/*fonts* pretty/*fonts*]
          (repl/pretty-print-stack-trace actual t/*stack-trace-depth*))
        (puget/cprint actual cprint-options))
      (println (str "# ERROR-END " error-count " ###############################################")))))

(defn my-run-tests
  "Run tests in the dirs given the the collection. Stores the dirs in the eftest 
  *context* atom to help with resolving file paths."
  [dirs]
  (run-tests (find-tests dirs)))