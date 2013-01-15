(ns inspector.javert
  "Lifted straight out of swank-clojure."
  (:import (java.lang.reflect Field)))

(defn- indexed-values [obj]
  (apply concat
         (map-indexed (fn [idx val]
                        `(~(str "  " idx ". ") (:value ~val) (:newline)))
                      obj)))

(defn- label-value-line*
  ([label value] (label-value-line* label value true))
  ([label value newline?]
     (list* (str label) ": " (list :value value)
            (if newline? '((:newline)) nil))))

(defmacro label-value-line [& label-values]
  `(concat ~@(map (fn [[label value]]
                    `(label-value-line* ~label ~value))
                  label-values)))

(defn- named-values [obj]
  (apply concat
         (for [[key val] obj]
           `("  " (:value ~key) " = " (:value ~val) (:newline)))))

(defmulti inspect
  (fn known-types [obj]
    (cond
      (map? obj) :map
      (vector? obj) :vector
      (var? obj) :var
      (string? obj) :string
      (seq? obj) :seq
      (instance? Class obj) :class
      (instance? clojure.lang.Namespace obj) :namespace
      (instance? clojure.lang.ARef obj) :aref
      (.isArray (class obj)) :array)))

(defn inspect-meta-information [obj]
  (when (seq (meta obj))
    (concat
     '("Meta Information: " (:newline))
     (named-values (meta obj)))))

(defmethod inspect :map [obj]
  (concat
   (label-value-line
    ("Class" (class obj))
    ("Count" (count obj)))
   (inspect-meta-information obj)
   '("Contents: " (:newline))
   (named-values obj)))

(defmethod inspect :vector [obj]
  (concat
   (label-value-line
    ("Class" (class obj))
    ("Count" (count obj)))
   (inspect-meta-information obj)
   '("Contents: " (:newline))
   (indexed-values obj)))

(defmethod inspect :array [^"[Ljava.lang.Object;" obj]
  (concat
   (label-value-line
    ("Class" (class obj))
    ("Count" (alength obj))
    ("Component Type" (.getComponentType (class obj))))
   '("Contents: " (:newline))
   (indexed-values obj)))

(defmethod inspect :var [^clojure.lang.Var obj]
  (concat
   (label-value-line
    ("Class" (class obj)))
   (inspect-meta-information obj)
   (when (.isBound obj)
     `("Value: " (:value ~(var-get obj))))))

(defmethod inspect :string [obj]
  (concat
   (label-value-line
    ("Class" (class obj)))
   (list (str "Value: " (pr-str obj)))))

(defmethod inspect :seq [obj]
  (concat
   (label-value-line
    ("Class" (class obj)))
   (inspect-meta-information obj)
   '("Contents: " (:newline))
   (indexed-values obj)))


(defmethod inspect :default [obj]
  (let [^"[Ljava.lang.reflect.Field;" fields (. (class obj) getDeclaredFields)
        names (map #(.getName ^Field %) fields)
        get (fn [^Field f]
              (try (.setAccessible f true)
                   (catch java.lang.SecurityException e))
              (try (.get f obj)
                   (catch java.lang.IllegalAccessException e
                     "Access denied.")))
        vals (map get fields)]
    (concat
     `("Type: " (:value ~(class obj)) (:newline)
       "Value: " (:value ~obj) (:newline)
       "---" (:newline)
       "Fields: " (:newline))
     (mapcat
      (fn [name val]
        `(~(str "  " name ": ") (:value ~val) (:newline))) names vals))))

(defn- inspect-class-section [obj section]
  (let [method (symbol (str ".get" (name section)))
        elements (eval (list method obj))]
    (if (seq elements)
      `(~(name section) ": " (:newline)
        ~@(mapcat (fn [f] `("  " (:value ~f) (:newline))) elements)))))

(defmethod inspect :class [^Class obj]
  (apply concat (interpose ['(:newline) "--- "]
                           (cons `("Type: " (:value ~(class obj)) (:newline))
                                 (for [section [:Interfaces :Constructors
                                                :Fields :Methods]
                                       :let [elements (inspect-class-section
                                                       obj section)]
                                       :when (seq elements)]
                                   elements)))))

(defmethod inspect :aref [^clojure.lang.ARef obj]
  `("Type: " (:value ~(class obj)) (:newline)
    "Value: " (:value ~(deref obj)) (:newline)))

(defn ns-refers-by-ns [^clojure.lang.Namespace ns]
  (group-by (fn [^clojure.lang.Var v] (. v ns))
            (map val (ns-refers ns))))

(defmethod inspect :namespace [^clojure.lang.Namespace obj]
  (concat
   (label-value-line
    ("Class" (class obj))
    ("Count" (count (ns-map obj))))
   '("---" (:newline)
     "Refer from: " (:newline))
   (mapcat (fn [[ns refers]]
             `("  "(:value ~ns) " = " (:value ~refers) (:newline)))
           (ns-refers-by-ns obj))
   (label-value-line
    ("Imports" (ns-imports obj))
    ("Interns" (ns-interns obj)))))

(defmulti inspect-print-component first)

(defmethod inspect-print-component :newline [_]
  (prn))

(defmethod inspect-print-component :value [[_ & xs]]
  (doseq [x xs]
    (print (str x))))

(defmethod inspect-print-component :default [x]
  (print x))

(defn inspect-print [x]
  (doseq [component (inspect x)]
    (inspect-print-component component)))