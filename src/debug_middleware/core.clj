(ns debug-middleware.core 
 (:require 
  [cdt.ui :refer :all]
  [cheshire.core :as json]
  [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
  [clojure.tools.nrepl.transport :as t]
  [clojure.tools.nrepl.misc :refer [response-for returning]]
  [clojure.tools.logging :refer :all]
  [clojure.core.async :refer [thread <!!]]
  [debug-middleware.jdi :as jdi]
  [debug-middleware.language-server :as lang]
  [pjstadig.humane-test-output :as humane-output])
 (:import 
  [com.sun.jdi Bootstrap]
  [com.sun.jdi AbsentInformationException]
  [com.sun.jdi.request BreakpointRequest]))
 
;; Returns a handler for operation.
(defmulti handle-msg (fn [handler msg] 
                      ; (println "Received message " msg)
                      (:op msg)))

(defmethod handle-msg "list-vars"
 [handler {:keys [op session id transport thread-name frame-index] :as msg}]
 (let [thread (jdi/get-thread-with-name thread-name)
       vars (try 
             (locals (ct) frame-index)
             (catch AbsentInformationException e
              [[][]]))
       vars (pr-str vars)]
  (t/send transport (response-for msg :status :done :vars vars))))
   
(defmethod handle-msg "list-frames"
 [handler {:keys [op session thread-name id transport] :as msg}]
 (let [frames (jdi/my-list-frames thread-name)]
  (t/send transport (response-for msg :status :done :frames frames))))

(defmethod handle-msg "list-threads"
 [handler {:keys [op session interrupt-id id transport] :as msg}]
 (let [threads (jdi/my-list-threads)]
  (t/send transport (response-for msg :status :done :threads threads))))

(defmethod handle-msg "get-source-paths"
  [handler {:keys [op session interrupt-id id transport source-files] :as msg}]
  (let [full-src-files (map lang/get-src-path source-files)]
    (t/send transport (response-for msg :status :done :paths full-src-files)))) 
  
(defmethod handle-msg "get-event"
 [handler {:keys [op session interrup-id id transport] :as msg}]
 (let [evt-map (<!! jdi/event-channel)]
  (t/send transport (response-for msg :status :done :event evt-map))))

(defmethod handle-msg "require-namespace"
  [handler {:keys [op namespace session interrup-id id transport] :as msg}]
  (let [msg (-> msg
                (assoc :op "eval" :code (str "(require '" namespace ")"))
                (dissoc :namespace))]
    (handler msg)))
       ;;  resp (handler msg)]
; (t/send transport (response-for msg :status "OK"))
    ;;(t/send transport (assoc resp :op "require-namespace" :namespace namespace))))

(defmethod handle-msg "set-breakpoint"
  [handler {:keys [op line path session interrupt-id id transport] :as msg}]
  (let [out (with-out-str (jdi/my-set-breakpoint path line))]
    (t/send transport (response-for msg :status :done :msg out))))
    
(defmethod handle-msg "clear-breakpoints"
  [handler {:keys [op session interrupt-id id transport path] :as msg}]
  (jdi/my-clear-breakpoints path)
  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "set-exception-breakpoint"
  [handler {:keys [op sesssion interrupt-id transport type class] :as msg}]
  (jdi/clear-all-exception-breakpoints)
  (when (contains? #{"all" "uncaught"} type)
    (jdi/set-exception-breakpoint type class))
  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "continue"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (jdi/my-continue)
  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "step-over"
  [handler {:keys [op session thread-name interrupt-id id transport] :as msg}]
  (jdi/my-step-over thread-name)
  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "step-into"
  [handler {:keys [op session thread-name interrupt-id id transport] :as msg}]
  (jdi/my-step-into thread-name)
  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "step-out"
  [handler {:keys [op session thread-name interrupt-id id transport] :as msg}]
  (jdi/my-step-out thread-name)
  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "get-completions"
 [handler {:keys [op session interrupt-id id transport ns src pos prefix] :as msg}]
 (let [completions (lang/get-completions ns prefix src pos)]
   (t/send transport (response-for msg :status :done :completions completions)))) 
  
(defmethod handle-msg "find-definition"
  [handler {:keys [op session interrupt-id id transport ns sym] :as msg}]
  (let [{:keys [path line error]} (lang/find-definition ns sym)]
    (if error
      (t/send transport (response-for msg :status :error :message error))
      (t/send transport (response-for msg :status :done :path path :line line)))))

(defmethod handle-msg "doc"
 [handler {:keys [op session interrupt-id id transport ns var] :as msg}]
 (try
  (let [doc-string (lang/get-doc ns var)]
    (if doc-string
      (t/send transport (response-for msg :status :done :doc doc-string))
      (t/send transport (response-for msg :status :done :doc "Failed to retrieve docstring."))))
  (catch Throwable e
   (t/send transport (response-for msg :status :done :doc "Failed to retrieve docstring")))))

(defmethod handle-msg "args"
 [handler {:keys [op session interrupt-id id transport ns var] :as msg}]
 (try
  (let [args (lang/get-args ns var)]
    (if (seq args)
      (t/send transport (response-for msg :status :done :args args))
      (t/send transport (response-for msg :status :done :args []))))
  (catch Throwable e
   (t/send transport (response-for msg :status :done :doc "Failed to retrieve args")))))

(defmethod handle-msg "signatures"
 [handler {:keys [op session interrupt-id id transport ns var] :as msg}]
 (try
  (let [sigs (lang/get-signatures ns var)]
    (if (seq sigs)
      (t/send transport (response-for msg :status :done :sigs sigs))
      (t/send transport (response-for msg :status :done :sigs []))))
  (catch Throwable e
   (t/send transport (response-for msg :status :done :doc "Failed to retrieve signatures")))))

(defmethod handle-msg "run-all-tests"
 [handler {:keys [op session interrupt-id seq-dirs par-dirs transport] :as msg}]
 (try
   (let [report (lang/run-all-tests par-dirs seq-dirs)]
     (println "ALL TESTS FINISHED")
     (t/send transport (response-for msg :status :done :report (json/generate-string report))))
   (catch Exception e
     (binding [*out* *err*]
       (println "PROBLEM RUNNING TESTS")
       (println (.getMessage e)))
     (.printStackTrace e)
     (t/send transport (response-for msg :status :error :err-msg (.getMessage e))))))

(defmethod handle-msg "run-tests-in-namespace"
 [handler {:keys [op session interrupt-id transport ns] :as msg}]
 (let [report (lang/run-tests-in-namespace ns)]
   (t/send transport (response-for msg :status :done :report (json/generate-string report)))))

(defmethod handle-msg "run-test"
 [handler {:keys [op session interrup-id transport ns test-name] :as msg}]
 (let [report (lang/run-test ns test-name)]
   (t/send transport (response-for msg :status :done :report (json/generate-string report)))))

(defmethod handle-msg "reval"
  [handler {:keys [op session interrupt-id id transport frame-num form] :as msg}]
  (let [val (jdi/my-reval frame-num form)
        f (read-string form)]
    (t/send transport (response-for msg :status :done :value val))))

(defmethod handle-msg "load-src-file"
  [handler {:keys [op session interrupt-id id transport path] :as msg}]
  (lang/load-source-file path)
  (t/send transport (response-for msg :status :done)))

; (defmethod handle-msg "load-file"
;  [handler {:keys [op session interrupt-id id transport] :as msg}]
;  ;; TODO figure out how to print response in debug console so we can
;  ;; print it there instead of in the REPL terminal window.
;  (lang/refresh)
;  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "refresh"
 [handler {:keys [op session interrupt-id id transport] :as msg}]
 ;; TODO figure out how to print response in debug console so we can
 ;; print it there instead of in the REPL terminal window.
 (lang/refresh)
 (t/send transport (response-for msg :status :done)))
;  (let [resp (with-out-str (lang/refresh))]
;    (t/send transport (response-for msg :status :done :msg resp))))

(defmethod handle-msg "super-refresh"
 [handler {:keys [op session interrupt-id id transport] :as msg}]
 (lang/super-refresh)
 (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "pid"
 [handler {:keys [op session interrupt-id id transport pid] :as msg}]
 (let [pid (lang/pid)]
   (t/send transport (response-for msg :status :done :pid pid))))

(defmethod handle-msg "reformat"
  [handler {:keys [op session interrupt-id id transport code] :as msg}]
  (let [rcode (lang/reformat-string code {})]
    (t/send transport (response-for msg :status :done :code rcode))))

(defmethod handle-msg "attach"
 [handler {:keys [op session interrupt-id id transport host port] :as msg}]
 (jdi/setup-debugger host port)
 (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "exit"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
 (jdi/exit)
 (t/send transport (response-for msg :status :exited)))

(defmethod handle-msg "fix-ns"
  [handler {:keys [op session interrupt-id id transport path] :as msg}]
  (let [source (lang/fix-ns path)]
    (t/send transport (response-for msg :status :done :value source))))

(defmethod handle-msg :default 
  [handler msg]
  (handler msg))
  
(defn debug-middleware
 "Lein middleware to handle debug requests." 
 [handler]
 (humane-output/activate!)
 (fn [msg]
  (handle-msg handler msg)))
    
(set-descriptor!
  #'debug-middleware
  {:expects #{}
   :requires #{"eval"}
   :handles {
             "attach"
                {:doc "Attach to a remove VM."
                 :requires {}
                 :returns {"result" "A map containig :status :done"}}
             "list-threads"
                {:doc "List the threads in the VM."
                 :requires {}
                 :returns {"result" "A map containing :status :done :threads threads"}}
             "reval"
                {:doc "Evalute an expression in the context of a thread frame."
                 :requires {}
                 :returns {"result" "The result message with :status :done"}}
             "get-event"
                {:doc "Request that the middleware send the next event as a response to this message."
                 :requires {}
                 :returns {"result" "A map containing :status :done :event event-map"}}
             "list-frames"
                {:doc "List the frames for a given thread."
                 :requires {"thread-name" "The id of the thread"}
                 :returns {"result" "A map containing :status :done :frames frames"}}
             "list-vars"
                {:doc "List the visible variables for a given stack frame."
                 :requires {"thread-name" "The id of the thread" "frame-index" "The index of the given frame"}
                 :returns {"result" "A map containing :status :done :vars variables"}}
             "require-namespace"
                {:doc "Require a namespace to force loading so it will be available for debugging"
                 :requires {"namespace" "The namespace to be required"}
                 :returns {"result" "A map containing :msg :ok or :error msg"}}
             "continue"
                {:doc "Continue after a breakpoint"
                 :requires {}
                 :returns {"result" "The result message with :status :done"}}
             "step-over"
                {:doc "Step over the next code block."
                 :requires {}
                 :returns {"result" "The result message with :status :done"}}
             "step-into"
                {:doc "Step into the next code block."
                 :requires {}
                 :returns {"result" "The result message with :status :done"}}
             "step-out"
                {:doc "Step out of the current code block."
                 :requires {}
                 :returns {"result" "The result message with :status :done"}}
             "clear-breakpoints"
                {:doc "Clear the breakpoints for a given source file"
                 :requires {"path" "The path to the source file"}
                 :returns {"result" "The result message with :status :done"}}
             "set-breakpoint"
                {:doc "Set a breakpoint"
                 :requires {"path" "The path to the source file"
                            "line" "The line at which to set the breakpoint"}
                 :returns {"result" "The result message with :status :done"}}
             "load-src-file"
                {:doc "Load the clojure source file at the given path."
                 :requires {"path" "The path to the source file."}
                 :returns {"result" "The result meassage with :status :done"}}
             "refresh"
                {:doc "Refresh code that has changed."
                 :requires {}
                 :returns {"result" "A map containing :msg :ok or :error msg with :status :done"}}
             "super-refresh"
                {:doc "Reload all code."
                 :requires {}
                 :returns {"result" "A map containing :msg :ok or :error msg with :status :done"}}
             "run-all-tests"
                {:doc "Run all the tests in the project."
                 :requires {"par-dirs" "Directories containing tests to be run in parallel"
                            "sync-dirs" "Director"}
                 :returns {"result" 
                           "A map containing :status :done or :error with a list of errors, :report - a map containing a summery of the test resutls."}}
             "run-tests-in-namespace"
               {:doc "Run all tests in the given namespace."
                :requires {"ns" "The namespace for the tests"}
                :returns {"result"
                          "A map cointaining :status :done or :error with a list of errors, :report - a map containing a summery of the test resutls."}}
             "run-test"
                {:doc "Run a single test."
                 :requires {"ns" "The namespace containing the test"
                            "test-name" "The name of the test to be executed."}
                 :returns {"result"
                           "A map cointaining :status :done or :error with a list of errors, :report - a map containing a summery of the test resutls."}}
             "pid"
                {:doc "Returns the process id for the JVM process."
                 :requires {}
                 :returns {"result" "A map conttaining :status :done and :pid <process id> or :error with a list of errors."}}
             "reformat"
                {:doc "Returns a formatted version of the input code string."
                 :requires {"code" "The code to be formatted."}
                 :returns {"result" "A map conttaining :status :done and :code <reformatted code> or :error with a list of errors."}}
             "fix-ns"
               {:doc "Returns the source for the given file with the ns entry fixed (missing
                      requires fixed, etc.)"
                :requires {}
                :returns {"result" "A map containing :value - the reformatted source and
                          :staus :done"}}
             "doc"
                {:doc "Get the docstring for the given symbol."
                 :requires {"var" "The var for which to return the docstring"}
                 :returns {"result" "A map containing :msg docstring or :error msg with :status :done"}}}
            "args"
                {:doc "Get the list of arguments for the given function"
                 :requires {"var","The var for the function"}
                 :returns {"result" "A map containing :args arglist or :error msg with :status :done"}}
             "get-completions"
                {:doc "Returns a list of possible completions for the given prefix."
                 :requires {"prefix" "The characters entered by the user" 
                            "ns" "The namespace where the prefix was entered"
                            "src" "The source text of the file where the prefix was entered"
                            "pos" "THe character position in the source where the prefix begins"}
                 :returns {"result" "A map containing :status :done :completions completions"}}
             "find-definition"
                {:doc "Find the location where a symbol is defined."
                 :requires {"ns" "The namespace in which the search was executed." "sym" "The symbol to find"}
                 :returns {"result" "A map containig :status :done :path path :line line"}}})
  
  
