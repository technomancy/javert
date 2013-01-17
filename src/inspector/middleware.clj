(ns inspector.middleware
  (:require [inspector.javert :as javert]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.middleware.session :refer [session]]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]))

(defn lookup-inspect
  [ns sym]
  (with-out-str
    (if-let [namespace (find-ns sym)]
      (javert/inspect-print namespace)
      (if-let [var (ns-resolve ns sym)]
        (if (or (instance? Class var) (:macro (meta var)) (fn? @var))
          (javert/inspect-print var)
          (javert/inspect-print @var))
        (throw (ex-info (format "Could not resolve %s in namespace: %s" sym ns)
                        {:namespace ns :symbol sym}))))))

(defn wrap-inspect
  [handler]
  (fn [{:keys [op ns sym transport] :as msg}]
    (if (= op "inspect")
      (->> {:value (lookup-inspect (symbol ns) (symbol sym)) :status :done}
           (response-for msg)
           (transport/send transport))
      (handler msg))))

(set-descriptor! #'wrap-inspect
  {:requires #{#'session}
   :handles {"inspect" {:doc "Print the results of inspector.javert/inspect-print to stdout."
                        :requires {"symbol" "Inspect the value bound to this symbol."
                                   "ns" "Resolve the symbol in this namespace."}}}})