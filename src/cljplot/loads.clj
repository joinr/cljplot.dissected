(ns cljplot.loads)

(doseq [nm '[[cljplot.render :as r]
             [cljplot.build :as b]
             [cljplot.common :refer :all]
             [fastmath.interpolation :as in]
             [fastmath.stats :as stats]
             [clojure2d.color :as c]
             [cljplot.scale :as s]
             [fastmath.core :as m]
             [fastmath.random :as rnd]
             [cljplot.core :refer :all]
             [java-time :as dt]
             [clojure.string :as str]
             [clojure2d.core :as c2d]
             [clojure2d.pixels :as p]
             [fastmath.complex :as cx]
             [fastmath.fields :as f]
             [fastmath.vector :as v]
             [fastmath.gp :as gp]
             [fastmath.distance :as dist]
             [fastmath.kernel :as kk]
             ;;monkey patches to get this working, pending issues in repo.
             [cljplot.patches]]]
  (do (println nm)
      (time (eval `(require '~nm)))))
