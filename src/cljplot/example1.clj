(ns cljplot.example1
  (:require [cljplot.render :as r]
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
            [fastmath.kernel :as kk]))

;;if we evaluate the original example, we have a problem during
;;the builder process, when we try to eval the add-side steps.

;;Execution error (ArityException) at
;;fastmath.stats$eval10137$fn__10138/doInvoke (stats.clj:622).
;;Wrong number of args (1) passed to: fastmath.stats/eval10129/fn--10130



;;In this first example, we want to display a scatter plot, which will
;;have kernel density plots displayed for each axis in the margins....

;;for grins, we'd like to display our scatter plots
;;according to a color gradient to visually demonstrate
;;magnitude on a normalized scale.



;;gradient returns a function :: t -> color, where
;;t :: double, color :: Vec3

;;color is represented as a clojure2d vec3 type.
;;The input configuration (elided), defines
;;different gradient interpolation strategies,
;;as well as color(s) to gradiate between, etc.
(c/gradient-presets :two-heads-filonov)

(defn pixels->points [ps]
  (let [width (c2d/width ps)
        height (c2d/height ps)]
    ;;generate a bunch of random points spread across the
    ;;the bounds of the logo
    (->> (repeatedly #(v/vec2 (rnd/irand width) (rnd/irand height)))
         ;;retain the points that are visible in the pixels.
         (filter #(pos? (c/luma (apply p/get-color logo %))))
         ;;add some random gaussian nose to the points
         ;;to get some unique jitter.
         (map #(v/add % (v/generate-vec2 rnd/grand)))
         ;;grab 2500 of these generated points to create
         ;;a neat visual set that outlines the image.
         (take 2500))))
