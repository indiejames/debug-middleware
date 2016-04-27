(ns debug-middleware.core 
 (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
           [clojure.tools.nrepl.transport :as t]
           [clojure.tools.nrepl.misc :refer [response-for returning]]
           [clojure.core.async :refer [thread]]
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

(defmethod handle-msg "require-namespace"
  [handler {:keys [op namespace session interrup-id id transport] :as msg}]
  (println "REQUIRING NAMESPACE " namespace)
  (let [msg (assoc msg :op "eval" :code (str "(require '" namespace ")"))
        resp (handler msg)]
    (assoc resp :op "require-namespace" :namespace namespace)))

(defmethod handle-msg "set-breakpoint"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (println "SETTING BREAKPOINT")
  (println "MSG: " msg)
  (let [{:keys [line path]} msg]
    (jdi/set-breakpoint @vm-atom path line)
    (t/send transport (response-for msg :status "BREAKPOINT SET"))))

(defmethod handle-msg "continue"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (println "Continue request received.")
  (jdi/continue @vm-atom)
  (t/send transport (response-for msg :status "CONTINUED")))
 
(defmethod handle-msg :default 
  [handler msg]
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
   :requires #{}
   :handles {"require-namespace"
                {:doc "Require a namespace to force loading so it will be available for debugging"
                 :requires {"namespace" "The namespace to be required"}
                 :returns {"result" "A map containing :msg :ok or :error msg}"}}
             "continue"
                {:doc "Continue after a breakpoint"
                 :requires {}
                 :returns {"result" "The result message"}}
             "set-breakpoint"
                {:doc "Set a breakpoint"
                 :requires {"path" "The path to the source file"
                            "line" "The line at which to set the breakpoint"}
                 :returns {"result" "The result message"}}}}) 
  
  
