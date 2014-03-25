(ns tarbean.intermediate
  (:require [clojure.walk :refer [postwalk prewalk walk prewalk-replace]]))


(defmacro node [name]
  {:name name
   :type :node
   :symbol `(fn [] (symbol (str ~name)))})

(def example-num-node
  {:name "1"
   :value nil
   :condition (fn [input]
                 (if (<= (get input "CAT") 17.5)
                   :2
                   :3))})

(def example-cat-node
  {:name "2"
   :value nil
   :leaf false
   :condition (fn [input]
                (get {"1" :node3 "2" :node4} input :node3))})

(def example-schema
  [{:name "1"
    :leaf false
    :condition (fn [input]
                 (if (<= (get input "CAT") 17.5)
                   (node 2)
                   (node 3)))}
   {:name "2"
    :leaf false
    :condition (fn [input]
                 (get {"1" (node 3) "2" (node 4)}
                      (get input "price")
                      (node 3)))}
   {:name "3"
    :value 7
    :leaf true}
   {:name "4"
    :value 8
    :leaf true}])

(defn walk-func [x]
  )

(defn tester [form]
  (let [nodes (atom {})]
    (prewalk (fn [x]
               (if (and (map? x) (= :node (:type x)))
                 (let [node-id (keyword (str "node-" (:name x)))]
                   (do (swap! nodes  assoc node-id (:symbol x))
                       node-id))
                 x))
             `(fn [input]
                (if (<= (get input "CAT") 17.5)
                  ~(node 2)
                  ~(node 3))))
    @nodes))
