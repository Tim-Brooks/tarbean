(ns tarbean.node
  (:require [clojure.walk :refer [postwalk-replace prewalk]]
            [tarbean.intermediate :as intermediate]))

(set! *warn-on-reflection* true)

(definterface INode
  (isLeaf [])
  (getId [])
  (getValue [])
  (setChildNodes [nodes])
  (nextNode [features]))

(deftype LeafNode [id value]
  INode
  (isLeaf [_] true)
  (getId [_] id)
  (getValue [_] value)
  (setChildNodes [_ _] nil)
  (nextNode [_ _] nil))

(defn- generate-branch-node
  [id condition children]
  (let [child-nodes (map (fn [c] (with-meta
                                  c
                                  {:unsynchronized-mutable true}))
                         (vals children))
        condition-with-symbols (postwalk-replace children condition)
        type-name (str "Branch" id)
        node-vec-sym (gensym "nodes")]
    [`(deftype ~(symbol type-name) [id# value# ~@child-nodes]
        INode
        (isLeaf [_] false)
        (getId [_] id#)
        (getValue [_] value#)
        (setChildNodes [_ ~node-vec-sym]
          (do
            ~@(for [[child idx] (map vector child-nodes (range (count child-nodes)))]
                `(set! ~child (nth ~node-vec-sym ~idx)))))
        (nextNode [_ features#] (~condition-with-symbols features#)))
     {:id type-name :node-order (vec (map str child-nodes))}]))

(defn- parse-children&condition [form]
  (let [nodes (atom {})
        condition (prewalk (fn [x]
                             (if (and (map? x) (= :node (:type x)))
                               (let [id (:id x)
                                     node-id (keyword (str "node-" id))]
                                 (do (swap! nodes
                                            assoc
                                            node-id
                                            (symbol (str id)))
                                     node-id))
                               x))
                           form)]
    [condition @nodes]))

(defn- build-branches [branches]
  (for [b branches]
    (let [id (:id b)
          value (:value b)
          [condition children] (parse-children&condition (:condition b))
          [def-type runtime-data] (generate-branch-node id
                                                        condition
                                                        children)
          type-symbol (symbol (str "->" (:id runtime-data)))]
      [def-type {:id id :value value :symbol type-symbol
                 :children (:node-order runtime-data)}])))

(defn- instantiate-branch [{:keys [id value symbol children]}]
  `{~id {:instance (~symbol ~id ~value ~@(repeat (count children) nil))
         :children ~children}})

(defn- instantiate-leaf [{:keys [id value]}]
  `{~id {:instance (->LeafNode ~id ~value)}})

(defn- construct-tree [root-id node-map]
  (do
     (doseq [{:keys [instance children]} (vals node-map)]
       (when-let [child-instances (map #(-> (get node-map %) :instance)
                                       children)]
         (.setChildNodes ^INode instance child-instances)))
     (get-in node-map [root-id :instance])))

(defmacro build-tree [raw-tree]
  (let [tree-var (var-get (resolve raw-tree))
        root-id (:id (first (filter :root tree-var)))
        branch-maps (filter #(not (:leaf %)) tree-var)
        leaf-maps# (filter :leaf tree-var)
        branches# (build-branches branch-maps)]
    `(do
       ~@(map #(nth % 0) branches#)
       (construct-tree ~root-id
                       ~(merge (into {} (map #(instantiate-branch (nth % 1))
                                             branches#))
                               (into {} (map instantiate-leaf leaf-maps#)))))))


(defn result [tree features]
  (loop [^INode current-node tree]
    (if (.isLeaf current-node)
      (.getValue current-node)
      (recur (.nextNode current-node features)))))

(def a (build-tree intermediate/example-tree))

