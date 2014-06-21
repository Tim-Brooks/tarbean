(ns tarbean.benchmark
  (:require [criterium.core :as crit]
            [tarbean.node :as node]))

(set! *warn-on-reflection* true)

(definterface ITest
  (getX []))

(deftype TestThing [a b c d e f]
  ITest
  (getX [_] a))

(def this-vec [1 2 3 4 5 6 7 8 9 10])

(defn bench []
  (let [thing ^TestThing (->TestThing "a" "b" "c" "d" "e" "f")
        thing2 {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6}
        thing3 (to-array this-vec)]
      (crit/with-progress-reporting
        (crit/quick-bench
         #_(do (aget thing3 1)
             (aget thing3 2)
             (aget thing3 3)
             (aget thing3 4)
             (aget thing3 5)
             (aget thing3 6)
             (.a thing)
             (.b thing)
             (.c thing)
             (.d thing)
             (.e thing)
             (.f thing))
         #_(do (:a thing2)
             (:b thing2)
             (:c thing2)
             (:d thing2)
             (:e thing2)
             (:f thing2))
         (dotimes [n 10]
           (node/result node/a {"name" "Kvothe" "hands" 2 "amount" 12}))))))
