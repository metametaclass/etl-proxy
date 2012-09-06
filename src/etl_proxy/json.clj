;; Copyright (c) 2012  Malyshev Artem  {-proofit404@gmail.com-}

(ns etl-proxy.json
  "Convert json markup language into graph data structure."
  (:use [cheshire.core]))

;; USEME:
;; <pre><code>
;; (parse-string (slurp "example.json") true)
;; </code></pre>
