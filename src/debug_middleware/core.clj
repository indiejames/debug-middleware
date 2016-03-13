(ns debug-middleware.core 
 (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
           [clojure.tools.nrepl.transport :as t]
           [clojure.tools.nrepl.misc :refer [response-for returning]])
 (:import com.sun.jdi.Bootstrap))
 
(def vm-manager-atom
 "Atom to hold the virtual machine manager"
 (atom nil))
 
(defn setup-debugger
 "Intialize the debugger."
 []
 (reset! vm-manager-atom (com.sun.jdi.Bootstrap/virtualMachineManager))
 (let [attachingConnectors (.attachingConnectors @vm-manager-atom)
       connector (some (fn [ac]
                          (when (= "dt_socket")
                                (-> ac .transport .name)
                            ac))
                       attachingConnectors)
       params-map (when connector (.defaultArguments connector))
       port-arg (when params-map (get params-map "port"))
       _ (when port-arg (.setValue port-arg 8030))]
   (when-let [vm (when port-arg (.attach connector params-map))]
     (println "Attached to process " (.name vm))
     (let [ref-types (.allClasses vm)
           ref-type (some (fn [rt]
                             (let [strata (set (.availableStrata rt))]
                               (when (contains? strata "Clojure")
                                 rt)))
                          ref-types)
           strata (when ref-type (.availableStrata ref-type))]
       (when strata (println (.name ref-type) ":  Strata: " strata)))
     vm)))
     
;; Returns a handler for operation.
(defmulti handle-msg (fn [handler msg] (:op msg)))

(defmethod handle-msg "set-breakpoint-bak"
  [handler msg]
  (println "SETTING BREAKPOINT")
  (println msg)
  (let [res (handler msg)]
   (println res)
   (assoc res :result "BREAKPOINT SET")))
   
(defmethod handle-msg "set-breakpoint"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (println "SETTING BREAKPOINT")
  (t/send transport (response-for msg :status "BREAKPOINT SET")))

(defmethod handle-msg :default 
  [handler msg]
  (handler msg))

  
(defn debug-middleware
 "Lein middleware to handle debug requests." 
 [handler]
 ;; Initialize the debugger.
 (setup-debugger)
 
 (fn [msg] 
  (handle-msg handler msg)))
    
(set-descriptor!
  #'debug-middleware
  {:expects #{}
   :requires #{}
   :handles {"set-breakpoint"
                {:doc "Set a breakpoint"
                 :requires {"path" "The path to the source file"
                            "line" "The line at which to set the breakpoint"}
                 :returns {"result" "The result message"}}}}) 
  
  
