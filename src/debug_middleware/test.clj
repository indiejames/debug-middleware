(ns debug-middleware.test
  "Provides enhanced test output."
  (:require
    [clojure.string :as str] 
    [eftest.report.pretty :as pretty :refer [report]]
    [eftest.report :as r]
    [eftest.runner :refer [find-tests run-tests]]
    [clojure.test :as t]))

(def ^:dynamic *test-dirs*
  "Atom to store the current test directories for the currently running tests."
  (atom []))

(defn stacktrace-file
  "Get the full path to the file for the currently failing test."
  [stacktrace file]
  (when (seq stacktrace)
    (let [^StackTraceElement s (first stacktrace)
          class-name (.getClassName s)]
      (-> class-name 
          (str/replace #"\$.*" "") 
          (str/replace "." "/")
          (str ".clj")))))


; (defmethod report :fail [{:keys [message expected] :as m}]
;   (test/with-test-out
;     (test/inc-report-counter :fail)
;     (print *divider*)
;     (println (str (:fail *fonts*) "FAIL" (:reset *fonts*) " in") (testing-vars-str m))
;     (when (seq test/*testing-contexts*) (println (test/testing-contexts-str)))
;     (when message (println message))
;     (if (= (first expected) '=)
;       (equals-fail-report m)
;       (predicate-fail-report m))))

(defn- walk [dirpath sub-path]
  "Walk the directory and subdirectories looking for a match to the pattern"
  (doall (filter #(str/ends-with? (.getAbsolutePath %) sub-path)
                 (file-seq (clojure.java.io/file dirpath)))))

(defn- resolve-file
  "Search through the given dirs for a path matching the given relative path."
  [dirs path]
  (-> (keep #(seq (walk % path)) dirs)
      ffirst
      .getAbsolutePath))

(defmethod report :fail
  [m]
  ;; Replace the filename with a full path
  (let [test-dirs @*test-dirs*
        file (stacktrace-file (drop-while
                                #(let [cl-name (.getClassName ^StackTraceElement %)]
                                   (or (str/starts-with? cl-name "java.lang.")
                                       (str/starts-with? cl-name "clojure.test$")
                                       (str/starts-with? cl-name "debug_middleware.test$")
                                       (str/starts-with? cl-name "eftest")
                                       (str/starts-with? cl-name "clojure.lang.MultiFn")))
                                (.getStackTrace (Thread/currentThread)))
                              (:file m))
        file (resolve-file test-dirs file)
        m (assoc m :file file)
        fail-count (:fail @t/*report-counters*)]
    (t/with-test-out
      (t/inc-report-counter :fail)
      (println (str "# FAIL-START " fail-count " #############################################"))
      (println "\nFAIL in" (t/testing-vars-str m))
      (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
      (when-let [message (:message m)] (println message))
      (println "expected:" (pr-str (:expected m)))
      (println "  actual:" (pr-str (:actual m)))
      (println (str "# FAIL-END " fail-count " ###############################################")))))

(defn my-run-tests
  "Run tests in the dirs given the the collection. Stores the dirs in the eftest 
  *context* atom to help with resolving file paths."
  [dirs]
  (swap! *test-dirs* (constantly dirs))
  (run-tests (find-tests dirs)))
  
(comment
  (my-run-tests ["test"])
  (+ 1 2))
