(ns cljplot.dissected.example1
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


;;The definition of leverages the underlying
;;clojure2d libraries, to instantiate a canvas
;;and render a customized logo reading "cljplot".
;;There are (expectedly) a bunch of minutae
;;regarding options for font and the like.
;;The canvas is then rendered to a wrapped
;;clojure2d.pixels.Pixels object, which is
;;effectively a wrapper around an int array.
;;There's a companion protocol
;;clojure2d.pixels.PixelsProto the defines
;;coercions to/from the pixels abstraction,
;;to enable compatibility with external
;;types, like java's BufferedImage.

;;A note on clojure2d:
;;Many of the operations are defined for,
;;relatively hardcore pixel-based operations
;;like filters, blurs, and other nice effects
;;for generative imagery.  The higher-level
;;canvas abstractions work down on this.

;;There's a canvas abstraction, which provides
;;a wrapped record around the java.awt.Graphics2D
;;rendering and some protocol access (e.g.
;;to get an image, via the ImageProto protocol),
;;with the ability to then hand off to the
;;pixel-based abstraction.  So, users are
;;able to define custom drawing using
;;primitive canvas operations.

(def logo (p/to-pixels (c2d/with-oriented-canvas-> :bottom-left+ (c2d/canvas 200 100)
                         (c2d/set-font "Raleway Thin")
                         (c2d/set-background :black)
                         (c2d/set-color :white)
                         (c2d/set-font-attributes 60)
                         (c2d/text "cljplot" 20 70))))

;;we can see the logo if we render the image...
;;cljplot.core provides a convenenience wrapper
;;for clojure2d.extra.utils/show-image, cljplot.core/show
;;This will coerce the logo to an image, which is
;;then rendered onto a window (concretely a JFrame).

;;interesting enough, our rendered logo is "upside down"
;;due to the orientation of the image.  this will
;;be corrected by cljplot for normal rendering..
(comment
(cljplot.core/show logo)
)



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
