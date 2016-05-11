(ns debug-middleware.core 
 (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
           [clojure.tools.nrepl.transport :as t]
           [clojure.tools.nrepl.misc :refer [response-for returning]]
           [clojure.core.async :refer [thread <!!]]
           [debug-middleware.jdi :as jdi])
 (:import com.sun.jdi.Bootstrap
          com.sun.jdi.request.BreakpointRequest))
 
(def vm-atom
 "Atom to hold the virutal machine"
 (atom nil))
 
;; Returns a handler for operation.
(defmulti handle-msg (fn [handler msg] 
                      (println "Received message " msg)
                      (:op msg)))

(defmethod handle-msg "list-vars"
 [handler {:keys [op session id transport thread-name frame-index] :as msg}]
 (println "LISTING VARS")
 (let [vars (jdi/list-vars @vm-atom thread-name frame-index)]
  (println "VARS: " vars)
  (t/send transport (response-for msg :status :done :vars vars))))
   
(defmethod handle-msg "list-frames"
 [handler {:keys [op session thread-name id transport] :as msg}]
 (println "LISTING FRAMES")
 (let [frames (jdi/list-frames @vm-atom thread-name)]
  (t/send transport (response-for msg :status :done :frames frames))))

(defmethod handle-msg "list-threads"
 [handler {:keys [op session interrup-id id transport] :as msg}]
 (println "LISTING THREADS")
 (let [threads (jdi/list-threads @vm-atom)]
  (t/send transport (response-for msg :status :done :threads threads))))
  
(defmethod handle-msg "get-event"
 [handler {:keys [op session interrup-id id transport] :as msg}]
 (println "GETTING EVENT")
 (let [evt-map (<!! jdi/event-channel)]
  (t/send transport (response-for msg :status :done :event evt-map))))

(defmethod handle-msg "require-namespace"
  [handler {:keys [op namespace session interrup-id id transport] :as msg}]
  (println "REQUIRING NAMESPACE " namespace)
  (let [msg (-> msg
                (assoc :op "eval" :code (str "(require '" namespace ")"))
                (dissoc :namespace))]
    (handler msg)))
       ;;  resp (handler msg)]
; (t/send transport (response-for msg :status "OK"))
    ;;(t/send transport (assoc resp :op "require-namespace" :namespace namespace))))

(defmethod handle-msg "set-breakpoint"
  [handler {:keys [op line path session interrupt-id id transport] :as msg}]
  (println "SETTING BREAKPOINT")
  (println "MSG: " msg)
  (jdi/set-breakpoint @vm-atom path line)
  (t/send transport (response-for msg :status :done)))
    
(defmethod handle-msg "clear-breakpoints"
  [handler {:keys [op path session interrupt-id id transport] :as msg}]
  (println "CLEARING BREAKPOINTS")
  (println "MSG: " msg)
  (jdi/clear-breakpoints @vm-atom path)
  (t/send transport (response-for msg :status :done)))

(defmethod handle-msg "continue"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (println "Continue request received.")
  (jdi/continue @vm-atom)
  (t/send transport (response-for msg :status :done)))
 
(defmethod handle-msg :default 
  [handler msg]
  (println "USING DEFAULT HANDLER")
  (handler msg))
  
(defn debug-middleware
 "Lein middleware to handle debug requests." 
 [handler]
 (reset! vm-atom (jdi/setup-debugger (System/getenv "CLOJURE_DEBUG_JDWP_PORT")))
 (fn [msg]
  (handle-msg handler msg)))
    
(set-descriptor!
  #'debug-middleware
  {:expects #{}
   :requires #{"eval"}
   :handles {"list-threads"
                {:doc "List the threads in the VM."
                 :requires {}
                 :returns {"result" "A map containing :status :done :threads threads"}}
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
                 :returns {"result" "A map containing :msg :ok or :error msg}"}}
             "continue"
                {:doc "Continue after a breakpoint"
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
                 :returns {"result" "The result message with :status :done"}}}})
  
  
