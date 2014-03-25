(ns tarbean.node
  (:require [clojure.walk :refer [postwalk-replace]]
            [tarbean.intermediate :refer [example-schema node]]))

(set! *warn-on-reflection* true)

(definterface INode
  (isLeaf [])
  (getName [])
  (getValue [])
  (nextNode [features]))

(deftype LeafNode [name value]
  INode
  (isLeaf [_] true)
  (getName [_] name)
  (getValue [_] value)
  (nextNode [_ _] nil))

(defn generate-branch-node
  [condition children]
  (let [children->symbols (into {} (map (fn [[k v]] {k (-> v str symbol)}) children))
        child-nodes (map (fn [c] (with-meta
                                  c
                                  {:unsynchronized-mutable true}))
                         (vals children->symbols))
        condition-with-symbols (postwalk-replace children->symbols condition)
        type-name (symbol (str "Branch" (count children)))]
    `(deftype ~type-name [name# value# ~@child-nodes]
       INode
       (isLeaf [_] false)
       (getName [_] name#)
       (getValue [_] value#)
       (nextNode [_ features#] (~condition-with-symbols features#)))))

(defn resolve-children [branch-map]
  (let [children-count:branches (group-by (fn [x] (:child-count x)) branch-map)]
    ))

(defmacro build-tree [raw-tree]
  (let [branch-maps (filter #(not (:leaf %)) (var-get (resolve raw-tree)))]
    (cons `do
          (for [b branch-maps]
            (generate-branch-node (:condition b) {})))))

(build-tree example-schema)

