(ns debug-middleware.core 
 (:require [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
           [clojure.tools.nrepl.transport :as t]
           [clojure.tools.nrepl.misc :refer [response-for returning]]
           [clojure.core.async :refer [thread]])
 (:import com.sun.jdi.Bootstrap
          com.sun.jdi.request.BreakpointRequest))
 
(def vm-manager-atom
 "Atom to hold the virtual machine manager"
 (atom nil))
 
(def vm-atom
 "Atom to hold the virutal machine"
 (atom nil))
 
(defn listen-for-events
  "List for events on the event queue and handle them."
  [evt-queue]
  (println "Listening for events....")
  (loop [evt-set (.remove evt-queue)]
    (println "Got an event............")
    (let [events (iterator-seq (.eventIterator evt-set))]
      (println "Handling event ==============================")
      (doseq [evt events
               :let [evt-req (.request evt)]]
        (cond 
          (instance? BreakpointRequest evt-req)
          (let [line (-> evt-req .location .lineNumber)]
            (println "Breakpoint hit at line " line))
          
          :default
          (println "Unknown event=============================="))))
    (recur (.remove evt-queue))))
 
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
     (let [evt-req-mgr (.eventRequestManager vm)
           evt-queue (.eventQueue vm)]
       (thread (listen-for-events evt-queue)))
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
(defmulti handle-msg (fn [handler msg] 
                      (println "Received message " msg)
                      (:op msg)))

(defmethod handle-msg "require-namespace"
  [handler {:keys [op namespace session interrup-id id transport] :as msg}]
  (println "REQUIRING NAMESPACE " namespace)
  (let [msg (assoc msg :op "eval" :code (str "(require '" namespace ")"))
        resp (handler msg)]
    (assoc resp :op "require-namespace" :namespace namespace)))
    
(defn ref-type-has-src-path?
 "Returns true if the given reference type has a src file matching the given path."
 [ref-type src-path]
 (try 
    (when-let [src-paths (.sourcePaths ref-type "Clojure")]
      (some (fn [path] (.endsWith src-path path)) 
            src-paths))
    (catch Exception e)))

(defn ref-type-matching-location
  "Returns the matching line location for the reference type, or nil if none exists."
  [ref-type line]
  (let [locs (.allLineLocations ref-type)]
    (some (fn [loc] (when (= (.lineNumber loc "Clojure") line) loc)) 
          locs)))

(defn find-loc-for-src-line
  "Find the ref-type that matches the given src file path and line."
  [src-path line]
  (let [ref-types (.allClasses @vm-atom)]
    (some (fn [ref-type]
            (when (ref-type-has-src-path? ref-type src-path)
              (do
                (println "Ref type has src path.....")
                (ref-type-matching-location ref-type line))))
          ref-types)))

(defmethod handle-msg "set-breakpoint"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (println "SETTING BREAKPOINT")
  (println "MSG: " msg)
  (let [{:keys [line path]} msg
        loc (find-loc-for-src-line path line)]
     (when loc
       (let [_ (println "Found location...............")
             _ (println loc)
             evt-req-mgr (.eventRequestManager @vm-atom)
             breq (.createBreakpointRequest evt-req-mgr loc)
             ;; Changed this from SUSPEND_ALL to SUSPEND_EVENT_THREAD because I think it is freezing
             ;; the event queue processing thread as well. Not sure how the example I got this from
             ;; could work.
             _ (.setSuspendPolicy breq com.sun.jdi.request.BreakpointRequest/SUSPEND_EVENT_THREAD)
             _ (.enable breq)])))
    ;          evt-queue (.eventQueue @vm-atom)])))
        ; ref-types (.allClasses @vm-atom)]
    ; (doseq [ref-type ref-types]
    ;   (let [name (.name ref-type)]
    ;     ; (println "NAME: " name) 
    ;     (when (re-find #"repl_test" name)
    ;       (println ">>>>>>>>>>>>>>>> FOUND NAME: " name)
    ;       (try 
    ;      ;; get source paths for all the classes with strata type "Clojure"
    ;        (let [source-names (.sourceNames ref-type "Clojure")
    ;              source-paths (.sourcePaths ref-type "Clojure")]
    ;          (println "SOURCE NAMES: " source-names)
    ;          (println "SOURCE PATHS: " source-paths))
    ;        (catch Exception e
    ;          (println "Can't get source")))))))
       
     
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

(defmethod handle-msg "continue"
  [handler {:keys [op session interrupt-id id transport] :as msg}]
  (println "Continue request received.")
  (.resume @vm-atom)
  (t/send transport (response-for msg :status "CONTINUED")))
  

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
  
  
