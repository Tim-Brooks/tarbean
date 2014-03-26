(ns tarbean.proto
  (:require [clojure.walk :refer [prewalk]]))

(definterface INode
  (isLeaf []))

(defmacro condition
  {:forms '[(condition [args*] form*)]}
  [args & form]
  `'(fn ~args ~@form))

(defn- parse-children&condition [form]
  (let [nodes (atom {})
        condition (prewalk (fn [x]
                             (if (and (map? x) (= :node (:type x)))
                               (let [name (:name x)
                                     node-id (keyword (str "node-" name))]
                                 (do (swap! nodes
                                            assoc
                                            node-id
                                            (symbol (str name)))
                                     node-id))
                               x))
                           form)]
    [condition @nodes]))

(def lolz
  [{:name "2"
     :value nil
     :leaf false
     :condition  (condition [input]
                            (get {"1" {:name 3 :type :node}
                                  "2" {:name 4 :type :node}}
                                 input
                                 '         {:name 3 :type :node}))}])
(do
  (deftype Launch2 [name value]
        INode
        (isLeaf [_] true))
  (doseq [b lolz]
    (let [name (:name b)
          value (:value b)
          condition (parse-children&condition (:condition b))
          def-type (symbol (str "Branch" name))]
      (println "here"))))
