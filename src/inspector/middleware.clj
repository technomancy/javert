(ns inspector.middleware
  (:require [inspector.javert :as javert]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.middleware.session :refer [session]]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]))

(defn lookup-inspect
  [ns sym]
  (with-out-str
    (let [value (or (find-ns sym) (ns-resolve ns sym))]
      (if (or (instance? Class value) (instance? clojure.lang.Namespace value)
              ;; I'm not sure if I should be hard-coding the decision
              ;; to inspect the var in the two cases below, but the
              ;; vars have more valuable info than the values do in my
              ;; opinion.
              (:macro (meta value)) (fn? @value))
        (javert/inspect-print value)
        (javert/inspect-print @value)))))

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