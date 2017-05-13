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

(def ^:private clear-line (apply str "\r" (repeat 80 " ")))

(defn- testing-vars-str [{:keys [file line]}]
  (let [test-var (first t/*testing-vars*)]
    (str (:clojure-frame pretty/*fonts*) (-> test-var meta :ns ns-name) "/"
         (:function-name pretty/*fonts*) (-> test-var meta :name) (:reset pretty/*fonts*)
         " (" (:source pretty/*fonts*) file ":" line (:reset pretty/*fonts*) ")")))

(def report-data
  "Atom holding the data for tests that fail/error"
  (atom {:fail []
         :error []}))

(defn reset-report-data!
  "Reset the report data atom to empty vectors."
  []
  (swap! report-data (constantly {:fail [] :error []})))

(defn- save-report!
  "Save a failue or error report in the report-data atom."
  [failure type]
 (swap! report-data (fn [rep-data] (update-in rep-data [type] #(conj % failure)))))

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

(defn- failure-str
  "Returns a string with failure information."
  [m]
  (with-out-str
    (do
      (when-let [message (:message m)] (println message))
      (println "expected:" (puget/cprint-str (:expected m) cprint-options))
      (println "  actual:" (puget/cprint-str (first  (:actual m)) cprint-options))
      (let [diff-minus (-> m :diffs first (nth 1) first)
            diff-plus (-> m :diffs first (nth 1) (nth 1))]
        (println "    diff: -" (puget/cprint-str diff-minus cprint-options))
        (println "          +" (puget/cprint-str diff-plus cprint-options))))))

(defn- error-str 
  "Returns a stirng with error information."
  [{:keys [message expected actual] :as m}]
  (with-out-str
    (do
      (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
      (when message (println message))
      (println "expected:" (puget/cprint-str expected cprint-options))
      (print "  actual: ")
      (if (instance? Throwable actual)
        (binding [exception/*traditional* true, exception/*fonts* pretty/*fonts*]
          (repl/pretty-print-stack-trace actual t/*stack-trace-depth*))
        (puget/cprint actual cprint-options)))))
      

(defmethod report :fail
  [m]
  (let [fail-count (:fail @t/*report-counters*)]
    (t/with-test-out
      (t/inc-report-counter :fail)
      (println clear-line)
      (println (str (:fail pretty/*fonts*) "FAIL" (:reset pretty/*fonts*) " in") (testing-vars-str m))
      (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
      (when-let [message (:message m)] (println message))
      (println "expected:" (puget/cprint-str (:expected m) cprint-options))
      (println "  actual:" (puget/cprint-str (first  (:actual m)) cprint-options))
      (let [diff-minus (-> m :diffs first (nth 1) first)
            diff-plus (-> m :diffs first (nth 1) (nth 1))
            failure-report {:source (t/testing-vars-str m)
                            :description (failure-str m)}]
        (println "    diff: -" (puget/cprint-str diff-minus cprint-options))
        (println "          +" (puget/cprint-str diff-plus cprint-options))
        (save-report! failure-report :fail)))))

(defmethod report :error 
  [{:keys [message expected actual] :as m}]
  (let [error-count (:error @t/*report-counters*)]
    (t/with-test-out
      (t/inc-report-counter :error)
      (println clear-line)
      (println (str (:error pretty/*fonts*) "ERROR" (:reset pretty/*fonts*) " in") (testing-vars-str m))
      (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
      (when message (println message))
      (println "expected:" (puget/cprint-str expected cprint-options))
      (print "  actual: ")
      (if (instance? Throwable actual)
        (binding [exception/*traditional* true, exception/*fonts* pretty/*fonts*]
          (repl/pretty-print-stack-trace actual t/*stack-trace-depth*))
        (puget/cprint actual cprint-options))
      (let [error-report {:source (testing-vars-str m)
                          :description (error-str m)}]
        (save-report! error-report :error)))))