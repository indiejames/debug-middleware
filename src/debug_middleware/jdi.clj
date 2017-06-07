(ns debug-middleware.jdi
 "Functions that wrap the JDI debugging code."
 (:require [cheshire.core :refer :all]
           [clojure.core.async :as async :refer [chan thread go >!]]
           [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
           [clojure.tools.nrepl.transport :as t]
           [clojure.tools.nrepl.misc :refer [response-for returning]]
           [cdt.ui :refer :all])
 (:import com.sun.jdi.Bootstrap
           com.sun.jdi.request.EventRequest
           com.sun.jdi.request.BreakpointRequest
           com.sun.jdi.request.StepRequest
           com.sun.jdi.BooleanValue
           com.sun.jdi.StringReference
           com.sun.jdi.LongValue
           com.sun.jdi.VirtualMachine
           com.sun.tools.jdi.LongValueImpl
           com.sun.tools.jdi.ObjectReferenceImpl
           com.sun.tools.jdi.StringReferenceImpl
           com.sun.tools.jdi.BooleanValueImpl))
           
(def event-channel
 "Channel used to communicate events."
 (chan))
           
(defn my-list-threads
 "Returns the list of threads for the given VM."
 []
 (let [thread-list (list-threads)]
   (map #(.name %) thread-list)))
   
(defn get-thread-with-name
 "Returns the ThreadReference with the given name"
 [name]
 (some (fn [thread-ref] 
         (when (= name (.name thread-ref)) thread-ref)) 
       (.allThreads (vm))))

(defn get-frame
 "Get the frame at the given stack position for the named thread"
 [thread-name stack-pos]
 (let [thread-ref (get-thread-with-name (vm) thread-name)]
   (.frame thread-ref stack-pos)))
   
(defn my-list-frames
 "Returns a list of frames for the thread with the given name."
 [thread-name]
 (let [thrd (get-thread-with-name thread-name)]
  (map (fn [frame]
        (let [loc (.location frame)
              line (.lineNumber loc "Clojure")
              src-path (try (.sourcePath loc "Clojure")
                            (catch Exception e ""))
              src-name (try (.sourceName loc "Clojure")
                            (catch Exception e ""))]
           {:srcPath src-path
            :srcName src-name
            :line line})) 
       (.frames thrd))))
  
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

(defn my-reval
 "Evaluate a form in the context of a thread/frame and return the result."
 [frame-num form]
 (let [thd (ct)
       _ (when-not thd (println "THREAD IS NULL"))
        f (read-string form)
        val (safe-reval thd frame-num f true read-string)]
    val))
   
; (defn set-value
;  "Set a value for a local variable."
;  [vm, frame variable-name value]
;  ;; TODO - Right now this only works for long values.
;  (if-let [variable (.visibleVariableByName frame variable-name)]
;   (.setValue frame variable (LongValueImpl. vm value))
;   (println "No such variable: " variable-name)))

(defn find-loc-for-src-line
  "Find the ref-type that matches the given src file path and line."
  [src-path line]
  (let [ref-types (.allClasses (vm))]
    (some (fn [ref-type]
            (when (ref-type-has-src-path? ref-type src-path)
              (ref-type-matching-location ref-type line))))
    ref-types))
          
(defn my-set-breakpoint
 "Set a breakpoint"
 [src-path line]
 ;; TODO modify cdt to return a value I can check here to see if the set worked
 (line-bp src-path line))

(defn set-exception-breakpoint
 "Set a breakpoint for caught or uncaught exceptions."
 [type classStr]
 (let [type-keyword (keyword type)
       class (resolve (symbol classStr))]     
   (set-catch class type-keyword)))

(defn my-clear-breakpoints
 "Delete all the breakpoints in a given file."
 [path]
;  (delete-all-catches)
 (delelete-breakpoints-in-file path))
                         
(defn clear-all-exception-breakpoints
 "Deletes all breakpoints for exceptions."
 []
 (delete-all-catches))

; (defn- my-step
;  "Step into or over called functions. Depth must be either StepRequest.STEP_INTO or
;  StepRequest.STEP_OVER"
;   [thread-name depth]
;   (let [evt-req-mgr (.eventRequestManager (vm))
;         thread-ref (get-thread-with-name thread-name)
;         step-req (.createStepRequest evt-req-mgr thread-ref StepRequest/STEP_LINE depth)]
;    (.addCountFilter step-req 1) ;; one step only
;    (.setSuspendPolicy step-req com.sun.jdi.request.EventRequest/SUSPEND_EVENT_THREAD)
;    (.enable step-req)
;    (.resume (vm))))

(defn my-step-into
  "Step into called functions"
  [ thread-name]
  (let [th (get-thread-with-name thread-name)]
    (step th)))

(defn my-step-out
  "Step out of called functions"
  [ thread-name]
  (let [th (get-thread-with-name thread-name)]
    (finish th)))
    
(defn my-step-over
  "Step over called functions"
  [thread-name]
  ; (step (vm) thread-name StepRequest/STEP_OVER))
  (let [th (get-thread-with-name thread-name)]
    (step-over th)))
   
(defn my-continue
 "Resume execution of a paused VM."
 []
 (continue-vm))

(defn- handle-exception-event
  [evt]
  (let [tr (.thread evt)
        loc (.catchLocation evt)
        src (.sourceName loc)
        line (.lineNumber loc)
        evt-map (generate-string {:event-type "exception"
                                  :thread (.name tr)
                                  :src src
                                  :line line})]
    (go (>! event-channel evt-map))))

(defn- handle-breakpoint-event
  [evt]
  (let [tr (.thread evt)
        evt-req (.request evt)
        loc (.location evt-req)
        src (.sourceName loc)
        line (.lineNumber loc)
        evt-map (generate-string {:event-type "breakpoint"
                                  :thread (.name tr)
                                  :src src
                                  :line line})]
    (go (>! event-channel evt-map))))

(defn- handle-step-event
 [evt]
 (let [tr (.thread evt)
       loc (.location evt)
       src (.sourceName loc)
       line (.lineNumber loc)
       evt-map (generate-string {:event-type "step"
                                 :thread (.name tr)
                                 :src src
                                 :line line})]
            
    (go (>! event-channel evt-map))))
           
; (defn listen-for-events
;   "List for events on the event queue and handle them."
;   [evt-queue evt-req-mgr]
;   (println "Listening for events....")
;   (loop [evt-set (.remove evt-queue)]
;     (let [events (iterator-seq (.eventIterator evt-set))]
;       (doseq [evt events
;                :let [evt-req (.request evt)]]
;         ; (println "CDB MIDDLEWARE EVENT" evt)
;         (cond 
;           (instance? BreakpointRequest evt-req)
;           (let [tr (.thread evt)
;                 loc (.location evt-req)
;                 src (.sourceName loc)
;                 line (.lineNumber loc)
;                 evt-map (generate-string {:event-type "breakpoint"
;                                           :thread (.name tr)
;                                           :src src
;                                           :line line})]
;             (go (>! event-channel evt-map)))
            
;          (instance? StepRequest evt-req)
;          (let [tr (.thread evt)
;                frame (.frame tr 0)
;                loc (.location frame)
;                src (.sourceName loc)]
;            (println "At location " (.lineNumber loc))
;            (println "File: " src)
;            (flush)
;             ;; Need to remove a step request or we won't be able to make another one.
;            (.deleteEventRequest evt-req-mgr evt-req))
          
; :default
;           (println "Unknown event")
;    (recur (.remove evt-queue))

(defn- attach
  "Repeatedly try to attach using JDI."
  [host port num-tries]
  (loop [n num-tries]
    (when (and (nil? (try (cdt-attach host port)
                          true
                          (catch Exception e nil)))
               (> n 0))
          (Thread/sleep 1000)
          (recur (dec n)))))

(defn exit
  "Tell the attached JVM to exit using JDI." 
  []
 (.exit (vm) 0))

(defn setup-debugger
 "Intialize the debugger by attaching to another process to be debugged on the given port."
 [host port]
 (attach host port 1000)
 (when  (vm)
   (.setDefaultStratum (vm) "Clojure")
   (set-handler breakpoint-handler handle-breakpoint-event)
   (set-handler exception-handler handle-exception-event)
   (set-handler step-handler handle-step-event)))
