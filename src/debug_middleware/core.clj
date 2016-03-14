(ns debug-middleware.core 
 (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
           [clojure.tools.nrepl.transport :as t]
           [clojure.tools.nrepl.misc :refer [response-for returning]])
 (:import com.sun.jdi.Bootstrap
          com.sun.jdi.request.BreakpointRequest))
 
(def vm-manager-atom
 "Atom to hold the virtual machine manager"
 (atom nil))
 
(def vm-atom
 "Atom to hold the virutal machine"
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
     
(defn find-location-in-ref-type
 "Find a location within the given Reference type that matches the given
 source file path and line number"
 [ref-type line path]
 (let [locs (.allLineLocations ref-type)]))

(defn find-location
 "Find the Location that matches the line number and source given"
 [vm line path]
 (let [ref-types (.allClasses vm)
       cloj-ref-types (filter (fn [rt]
                                (let [strata (set (.availableStrata rt))]
                                   (contains? strata "Clojure")))
                              ref-types)]))
      

;; Returns a handler for operation.
(defmulti handle-msg (fn [handler msg] (:op msg)))

(defmethod handle-msg "set-breakpoint"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (println "SETTING BREAKPOINT")
  (let [line (get "line" msg)
        path (get "path" msg)
        ref-types (.allClasses @vm-atom)]
    (doseq [ref-type ref-types]
      (let [name (.name ref-type)]
        (println "NAME: " name) 
        (when (or (re-find #"repl-test" name) 
                  (re-find #"middleware" name))
          (println ">>>>>>>>>>>>>>>> FOUND NAME: " name))
        (try 
          (let [source-names (.sourceNames ref-type "Clojure")]
            (println "SOURCE NAMES: " source-names))
          (catch Exception e
            (println "Can't get source"))))))
     
    ;     cloj-ref-types (filter (fn [rt]
    ;                                (let [strata (set (.availableStrata rt))]
    ;                                   (contains? strata "Clojure")))
    ;                             ref-types)]
    ;  (some (fn [crt]
    ;          (let [locs (.allLineLocations crt)]
    ;                loc (some (fn [lc] (= line (.lineNumber lc)))
    ;                          locs)]
    ;             (when loc
    ;               (let [evt-reg-mgr (.eventRequestManager vm)
    ;                     breq (.createBreakPointRequest ev-req-mgr loc)
    ;                     _ (.setSupsendPolicy breq (.SUSPEND_ALL com.sun.jdi.request.BreakpointRequest))
    ;                     _ (.enable breq)
    ;                     evt-queue (.eventQueue vm)
    ;                     ]))))
    
  (t/send transport (response-for msg :status "BREAKPOINT SET")))

(defmethod handle-msg :default 
  [handler msg]
  (handler msg))

  
(defn debug-middleware
 "Lein middleware to handle debug requests." 
 [handler]
 ;; Initialize the debugger.
 (reset! vm-atom (setup-debugger))
 
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
  
  
