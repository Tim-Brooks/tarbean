(ns tarbean.intermediate
  (:require [clojure.walk :refer [postwalk prewalk walk prewalk-replace
                                  macroexpand-all]]))


(defmacro node [name]
  {:name name
   :type :node
   :symbol `(fn [] (symbol (str ~name)))})

(defmacro condition
  {:forms '[(condition [args*] form*)]}
  [args & form]
  `'(fn ~args ~@form))

(println (condition [input]
                      (if (<= (get input "CAT") 17.5)
                        {:name 2 :type :node}
                        {:name 3 :type :node})))

(def example-num-node
  {:name "1"
   :value nil
   :condition '(fn [input]
                 (if (<= (get input "CAT") 17.5)
                   :2
                   :3))})

(def example-cat-node
  [{:name "2"
    :value nil
    :leaf false
    :condition  (condition [input]
                           (get {"1" {:name 3 :type :node}
                                 "2" {:name 4 :type :node}}
                                input
                                {:name 3 :type :node}))}
   {:name "3"
    :value nil
    :leaf false
    :condition  (condition [input]
                           (get {"1" {:name 5 :type :node}
                                 "2" {:name 6 :type :node}})
                           input
                           {:name 6 :type :node})}])

(def example-schema
  [{:name "1"
    :leaf false
    :condition '(fn [input]
                  (if (<= (get input "CAT") 17.5)
                    (node 2)
                    (node 3)))}
   {:name "2"
    :leaf false
    :condition '(fn [input]
                  (get {"1" (node 3) "2" (node 4)}
                       (get input "price")
                       (node 3)))}
   {:name "3"
    :value 7
    :leaf true}
   {:name "4"
    :value 8
    :leaf true}])


