(ns rules-clj.main
  (:require [cheshire.core :as json]
            [rules-clj.inference :refer :all])
  (:gen-class))

(defn -main
  [& args]
  (when (not= (count args) 2)
    (println "./rules-clj <RULES> <INPUT>")
    (System/exit 1))
  (let [rules (json/parse-string (first args) true)
        input (json/parse-string (second args))
        infer (inferoutputs (pretreat rules) input)]
    (println (json/generate-string infer))))
