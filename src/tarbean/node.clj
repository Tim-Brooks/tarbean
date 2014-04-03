(ns tarbean.node
  (:require [clojure.walk :refer [postwalk-replace prewalk]]
            [tarbean.intermediate :refer [example-schema example-cat-node node]]))

(set! *warn-on-reflection* true)

(definterface INode
  (isLeaf [])
  (getName [])
  (getValue [])
  (setChildNodes [nodes])
  (nextNode [features]))

(deftype LeafNode [name value]
  INode
  (isLeaf [_] true)
  (getName [_] name)
  (getValue [_] value)
  (setChildNodes [_ _] nil)
  (nextNode [_ _] nil))

(defn- generate-branch-node
  [name condition children]
  (let [child-nodes (map (fn [c] (with-meta
                                  c
                                  {:unsynchronized-mutable true}))
                         (vals children))
        condition-with-symbols (postwalk-replace children condition)
        type-name (str "Branch" name)
        node-sym (gensym "nodes")]
    [`(deftype ~(symbol type-name) [name# value# ~@child-nodes]
        INode
        (isLeaf [_] false)
        (getName [_] name#)
        (getValue [_] value#)
        (setChildNodes [_ ~node-sym]
          (do
            ~@(for [[child idx] (map vector child-nodes (range (count child-nodes)))]
                `(set! ~child (nth ~node-sym ~idx)))))
        (nextNode [_ features#] (~condition-with-symbols features#)))
     {:name type-name :node-order (vec (map str child-nodes))}]))

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

(defn- build-branches [branches]
  (for [b branches]
    (let [name (:name b)
          value (:value b)
          [condition children] (parse-children&condition (:condition b))
          [def-type runtime-data] (generate-branch-node (:name b)
                                                        condition
                                                        children)
          type-symbol (symbol (str "->" (:name runtime-data)))]
      [def-type {:name name :value value :symbol type-symbol
                 :children (:node-order runtime-data)}])))

(defn- instantiate [{:keys [name value symbol children]}]
  `{~name {:instance (~symbol ~name ~value nil nil) :children ~children}})

(defn- construct-tree [node-map]
  (doseq [{:keys [instance]} (vals node-map)]))

(defmacro build-tree [raw-tree]
  (let [branch-maps (filter #(not (:leaf %)) (var-get (resolve raw-tree)))
        branches# (build-branches branch-maps)]
    `(do
       ~@(map #(nth % 0) branches#)
       (construct-tree ~(into {} (map #(instantiate (nth % 1)) branches#))))))

(build-tree example-cat-node)

