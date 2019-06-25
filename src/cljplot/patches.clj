(ns cljplot.patches
  (:require [cljplot.impl.line]))
;;NOTE: patch...

(comment ;no longer necessary
(in-ns 'cljplot.impl.line)
(defmethod prepare-data :density [_ data {:keys [kernel-bandwidth margins] :as conf}]
  (let [dens-data (extract-first data)
        f (if kernel-bandwidth
            (stats/kernel-density :default dens-data kernel-bandwidth)
            (stats/kernel-density :default dens-data))
        with-domain (assoc conf :domain (extend-domain-numerical (take 2 (stats/extent dens-data)) (or (:x margins) [0 0])))]
    [with-domain (prepare-data :function f with-domain)]))
(in-ns 'cljplot.patches)
)
