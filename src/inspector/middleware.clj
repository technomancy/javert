(ns inspector.middleware
  (:require [inspector.javert :as javert]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.middleware.session :refer [session]]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]))

;; I'm not sure if I should be hard-coding the decision to inspect the
;; var for macros and functions. Yet, in my opinion, the vars have
;; more valuable info than the values do in those cases.
(defn lookup
  [ns sym]
  (let [var (or (find-ns sym) (ns-resolve ns sym))]
    (if (or (instance? Class var) (instance? clojure.lang.Namespace var)
            (:macro (meta var)) (fn? @var))
      var
      @var)))

(defn wrap-inspect
  [handler]
  (fn [{:keys [op ns sym transport] :as msg}]
    (if (= op "inspect")
      (let [value (with-out-str (javert/inspect-print (lookup (symbol ns) (symbol sym))))]
        (transport/send transport (response-for msg :value value :status :done)))
      (handler msg))))

(set-descriptor! #'wrap-inspect
  {:requires #{#'session}
   :handles {"inspect" {:doc "Print the results of inspector.javert/inspect-print to stdout."
                        :requires {"sym" "Inspect the value bound to this symbol."
                                   "ns" "Resolve the symbol in this namespace."}}}})
