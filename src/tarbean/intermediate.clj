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

(def example-tree
  [{:id "1"
    :leaf false
    :condition (condition [input]
                          (get {"Tim" {:id "2" :type :node}
                                "Kvothe" {:id "3" :type :node}}
                               (get input "name" "Tim")))}
   {:id "2"
    :leaf false
    :condition  (condition [input]
                           (get {1 {:id "4" :type :node}
                                 2 {:id "5" :type :node}
                                 3 {:id "6" :type :node}}
                                (get  input "hands" 1)))}
   {:id "3"
    :leaf false
    :condition  (condition [input]
                           (if (> (get input "amount" 20) 15)
                             {:id "5" :type :node}
                             {:id "7" :type :node}))}
   {:id "4"
    :leaf true
    :value "California"}
   {:id "5"
    :leaf true
    :value "Missouri"}
   {:id "6"
    :leaf true
    :value "Imre"}
   {:id "7"
    :leaf true
    :value "Severen"}])

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


