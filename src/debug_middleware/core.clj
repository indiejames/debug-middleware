(ns debug-middleware.core 
 (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
           [clojure.tools.nrepl.transport :as t]
           [clojure.tools.nrepl.misc :refer [response-for returning]]
           [clojure.tools.logging :refer :all]
           [clojure.core.async :refer [thread <!!]]
           [debug-middleware.jdi :as jdi]
           [debug-middleware.language-server :as lang]
           [cdt.ui :refer :all])
 (:import com.sun.jdi.Bootstrap
          com.sun.jdi.request.BreakpointRequest))
 
;; Returns a handler for operation.
(defmulti handle-msg (fn [handler msg] 
                      ; (println "Received message " msg)
                      (:op msg)))

(defmethod handle-msg "list-vars"
 [handler {:keys [op session id transport thread-name frame-index] :as msg}]
;  (println "LISTING VARS")
 (let [thread (jdi/get-thread-with-name thread-name)
       vars (locals (ct) frame-index)
       vars (pr-str vars)]
  ; (println "VARS: " vars)
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
  (let [[path line] (lang/find-definition ns sym)]
   (t/send transport (response-for msg :status :done :path path :line line))))

(defmethod handle-msg "doc"
 [handler {:keys [op session interrupt-id id transport ns var] :as msg}]
 (try
  (let [doc-string (lang/get-doc ns var)]
    (if doc-string
      (t/send transport (response-for msg :status :done :doc doc-string))
      (t/send transport (response-for msg :status :done :doc "Failed to retrieve docstring."))))
  (catch Throwable e
   (t/send transport (response-for msg :status :done :doc "Failed to retrieve docstring")))))

(defmethod handle-msg "run-all-tests"
 [handler {:keys [op session interrupt-id transport] :as msg}]
 (lang/run-all-tests))

(defmethod handle-msg "run-tests-in-namespace"
 [handler {:keys [op session interrupt-id transport ns] :as msg}]
 (lang/run-tests-in-namespace ns))

(defmethod handle-msg "run-test"
 [handler {:keys [op session interrup-id transport ns test-name] :as msg}]
 (lang/run-test ns test-name))

(defmethod handle-msg "reval"
  [handler {:keys [op session interrupt-id id transport frame-num form] :as msg}]
  (let [val (jdi/my-reval frame-num form)
        f (read-string form)]
    (t/send transport (response-for msg :status :done :value val))))

(defmethod handle-msg "refresh"
 [handler {:keys [op session interrupt-id id transport] :as msg}]
 (let [resp (with-out-str (lang/refresh))]
   (t/send transport (response-for msg :status :done :msg resp))))

(defmethod handle-msg "attach"
 [handler {:keys [op session interrupt-id id transport port] :as msg}]
 (jdi/setup-debugger port)
 (t/send transport (response-for msg :status :done)))
 
(defmethod handle-msg :default 
  [handler msg]
  (handler msg))
  
(defn debug-middleware
 "Lein middleware to handle debug requests." 
 [handler]
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
             "refresh"
                {:doc "Refresh code that has changed."
                 :requires {}
                 :returns {"result" "A map containing :msg :ok or :error msg with :status :done"}}
             "run-all-tests"
                {:doc "Run all the tests in the project."
                 :requires {}
                 :returns {"result" "A map containing :status :done or :error with a list of errors."}}
             "run-test"
                {:doc "Run a single test."
                 :requires {"ns" "The namespace containing the test"
                            "test-name" "The name of the test to be executed."}
                 :returns {"result" "A map containing :status :done or :error with a list of errors."}}
             "doc"
                {:doc "Get the docstring for the given symbol."
                 :requires {"var" "The var for which to return the docstring"}
                 :returns {"result" "A map containing :msg docstring or :error msg with :status :done"}}
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
                 :returns {"result" "A map containig :status :done :path path :line line"}}}})
  
  
