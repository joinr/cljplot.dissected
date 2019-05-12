;;ns to explore individual examples,
;;and, by construction, and lift
;;them into higher-order examples..
(ns cljplot.dissected
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

;;This is some setup for consistency in the RNG....incidental
;;to plotting.

(rnd/set-seed! rnd/default-rng 2)


;;For this dive, we're going to start from a simplistic, hello-world
;;style construct (a dumb scatter plot, with 0 frills) and look at how
;;cljplot encodes plotting concepts in clojure data structures.
;;Hopefully, we get an understanding of the organization and
;;architecture along the way - e.g. less magic.


;;In cljplot, the basic ideas are defining plots by declaring
;;datasets, relating them to visual layers via concepts like scale and
;;color, and then defining charts by composing one or more plots into
;;a visual construct.

;;The simplest dataset is our good old friend
;;the sequence.

;;Let's define a helper to generate some random
;;[x y] coordinates....
(defn random-points [n width height]
  (repeatedly n (fn [] [(rand-int width) (rand-int height)])))


;;cljplot provides some helper functions (without sacrificing
;;lower-level control for the user) that serve to build out the
;;declarative rendering language we'll use for plotting.  For now, we
;;use an extremely minimal subset of this specification to define a
;;scatter plot.

;;Given a sequence of pairs (points), we leverage the cljplot.build
;;namespace to define a collection of 'series'.

;;Each series is conceptually a visual layer that relates some data to
;;a visual form, to include scaling, coloring, stroking, etc.  The
;;visual layers derived from series compose in order, so the resulting
;;visual will render the first, then the second, etc.  resulting in an
;;intuitive overlay.
(defn minimal-xyplot-spec [pairs]
  (-> (b/series [:grid] ;;just a background layer of gridlines...
                [:scatter pairs]) ;;a scatter plot from the xy pairs.
      ;;In order to leverage cljplot's smart helpers, we'll
      ;;ask ot to preprocess the series to "fill in the blanks"
      ;;for minutae like scales and other things, so that
      ;;we have sane defaults.  This is a commonly recurring theme.
      (b/preprocess-series)))

;;let's get a sample specification, which projects a dataset of 2
;;random points into the xy plane, with a standard grid background.
;;Note: the result of this specification is a map of clojure data; we
;;have no visual representation (e.g. no rendereed image) as of yet.
(def xyspec 
  (-> 2500
      (random-points 600 300)
      minimal-xyplot-spec))

;;We can verify that our specification actually renders to a nice plot
;;verily....  To do so, we need to render the plot to a canvas, and
;;then show the result.

(defn show-plot []
  (-> xyspec
      r/render-lattice
      show))

;;The result appears to be as expected...  a bunch of random points
;;scattered about, and somewhat boring in that there are no axes
;;labels, no coloring, no legend, etc.  cljplot is capable of far more
;;than this, but we're intentionally taking a minimal, constructive
;;approach to explore and understand the library better...


;;From a data perspective, all cljplot has done so far is build up a
;;map to define the configuration of our minimal xyplot.  This is
;;nothing more than a specification that will define how to render and
;;style the associated data.  The nice thing is that it's amenable to
;;exploration if we'd like to, and even programmatic
;;manipulation (it's just a map....we know how to munge maps).

;;The function clojplot.render/render-lattice; is a sort of
;;higher-order rendering function that can smartly layout multiple
;;plots as subplots over the visual space, and do interesting things
;;like add side/top plots and other useful graphical stuff.  Most
;;importantly, it projects the plot specification onto an actual
;;canvas - specifically a clojure2d canvas - which can then be shown
;;or saved via the cljplot.core/show function.

;;We'll discuss the innards and clojure2d relations
;;more later...

;;From a high level, we see the usual suspects:
(keys xyspec)
;;'(:series :cols :rows :extents :scales)

;;The plot specification should conform to
;;this schema [I think]:

`{;;mapping of [column row] to a specific plot spec, used for visual layout.
  :series [[col row] plot-spec]
  ;;count of the columns in the plot layout.    
  :cols    :int
  ;;count of rows in the plot layout.
  :rows    :int
  ;;The visual extents defined by axis (:x,:y), relative to
  ;;each series and axis type (e.g. numerical, categorical).
  ;;This provides bounding box information for each subplot
  ;;[I think].
  :extents  {axis {series-id [axis-type [lower upper]]}}
  ;;defines the series-specific scale used for
  ;;each axis.  Note: scales are themselves
  ;;specifications, and trivially modified.
  :scales  {axis {series-id scale-specification}}
  }

;;To show that more - much more - exists, and is accessible via helper
;;functions or plain-old clojure munging, here is the whole
;;specification:

;; {:series
;;  {[0 0]
;;   [[:grid
;;     nil
;;     {:x
;;      {:color [128.0 128.0 128.0 180.0],
;;       :stroke {:size 0.5, :cap :butt, :dash [4.0], :dash-phase 2.0}},
;;      :y
;;      {:color [128.0 128.0 128.0 180.0],
;;       :stroke {:size 0.5, :cap :butt, :dash [4.0], :dash-phase 2.0}},
;;      :position [0 0],
;;      :series-id 0,
;;      :chart-type :grid,
;;      :extent {:x nil, :y nil}}]
;;    [:scatter
;;     ([417 211] [513 84])
;;     {:stroke {:size #function[clojure.core/constantly/fn--5657]},
;;      :color #function[clojure.core/constantly/fn--5657],
;;      :size #function[clojure.core/constantly/fn--5657],
;;      :extent
;;      {:x [:numerical [412.2 517.8]], :y [:numerical [77.65 217.35]]},
;;      :chart-type :scatter,
;;      :margins {:x [0.05 0.05], :y [0.05 0.05]},
;;      :shape #function[clojure.core/constantly/fn--5657],
;;      :position [0 0],
;;      :series-id 1}]]},
;;  :cols 1,
;;  :rows 1,
;;  :extents
;;  {:x {0 [:numerical [412.2 517.8]]},
;;   :y {0 [:numerical [77.65 217.35]]}},
;;  :scales
;;  {:x
;;   {0
;;    {:domain [412.2 517.8],
;;     :fmt #function[clojure.core/str],
;;     :ticks
;;     (420.0 430.0 440.0 450.0 460.0 470.0 480.0 490.0 500.0 510.0),
;;     :scale
;;     {:start 412.2,
;;      :end 517.8,
;;      :type :linear,
;;      :forward #function[fastmath.core/make-norm/fn--6547],
;;      :inverse #function[clojure.core/partial/fn--5826],
;;      :info nil},
;;     :scale-def [:linear]}},
;;   :y
;;   {0
;;    {:domain [77.65 217.35],
;;     :fmt #function[clojure.core/str],
;;     :ticks
;;     (80.0
;;      90.0
;;      100.0
;;      110.0
;;      120.0
;;      130.0
;;      140.0
;;      150.0
;;      160.0
;;      170.0
;;      180.0
;;      190.0
;;      200.0
;;      210.0),
;;     :scale
;;     {:start 77.65,
;;      :end 217.35,
;;      :type :linear,
;;      :forward #function[fastmath.core/make-norm/fn--6547],
;;      :inverse #function[clojure.core/partial/fn--5826],
;;      :info nil},
;;     :scale-def [:linear]}}}}


;;examining the :series key, we see a mapping of [col row] to one or
;;more plot specifications.  From scene graph perspective, we can view
;;this as the resulting image being diced into multiple rows and
;;columns, with each [row col] coordinate defining a subdivision
;;within the image for the a resulting visual element (or subplot) to
;;be rendered to appropriately (e.g.  translated, scaled,
;;etc. to "fit" the plot into the area).

;;For each [row col] subplot, we then have a vector that defines the
;;plot order for one or more plot specifications.  Each plot
;;specification is defined by a data-driven API, encoded as a tagged
;;vector (by default).

;;The associated series vectors implicitly encode the arguments for a
;;multi-method, cljplot.common/render-graph, which provides a generic
;;way for arbitrary visual plots to be implemented.

(comment
  ;;  [t         data      config      chart-data]
  ;;  ~ 
  ;;  [plot-type plot-data plot-config chart-data]

;;where t:

;;plot-type, any valid method implementation of
;;cljplot.common/render-graph for the dispatch value t.

;;Currently known values are defined in the methods in cljplot.impl.*
;;namespaces, including
;;:scatter and friends....

  
;;where config
{:stroke {:size #function[clojure.core/constantly/fn--5657]},
 :color  #function[clojure.core/constantly/fn--5657],
 :size   #function[clojure.core/constantly/fn--5657],
 :extent
 {:x [:numerical [412.2 517.8]],
  :y [:numerical [77.65 217.35]]},
 :chart-type :scatter,
 :margins {:x [0.05 0.05], :y [0.05 0.05]},
 :shape #function[clojure.core/constantly/fn--5657],
 :position [0 0],
 :series-id 1
 :x :scale-map
 :y :scale-map
 }

;;where chart-data (typically supplied by a
;;the cljplot.core/render-lattice function:

{:w :number ;;width of the subcanvas
 :h :number ;;height of the subcanvas
 :x :number ;;global x coordinate of the origin of the subcanvas
 :y :number ;;global y coordinate of the origin of the subcanvas 
 :extent {:x :number ;;x-extents in local coordinates
          :y :number ;;y-extents in local coordinates
          }}


;;render-graph is responsible for clojure2d canvas set up (typially
;;via creating a new, isolated canvas), and computing an
;;anchor point.
;;THe result is a clojure2d Canvas record [I think].
{:canvas ;;resulting image/canvas to coerce to an image
 :anchor ;;x,y coordinates that anchor the plot
 }

;;ex:
;;a simple scatter plot with 2 points [417 211], and [513 84]
;;may be rendered thusly:

;;this dumb example sets up a simple xyplot, with 2 points.
;;it's basically doing all the low-level work that cljplot.build
;;does for us out of the box with the higher-level API....

;;One difference here is that we explicitly construct the
;;scales - derived from the original plot spec - by use
;;of the helper function cljplot.scales/scale-map.
(let [sx (s/scale-map  [:linear]
                       {:ticks [420.0 430.0 440.0 450.0 460.0 470.0 480.0 490.0 500.0 510.0]
                        :domain [412.2 517.8]
                        :fmt str})       
      sy  (s/scale-map  [:linear]
                        {:ticks
                         [80.0 90.0 100.0 110.0 120.0 130.0 140.0 150.0 160.0 170.0
                          180.0 190.0 200.0 210.0]
                         :domain [77.65 217.35]
                         :fmt str})]
  (-> (render-graph :scatter [[417 211] [513 84]]
                    {:stroke {:size (constantly 10)}  
                     :color  (constantly :red) 
                     :size   (constantly 10) 
                     :extent
                     {:x [:numerical [412.2 517.8]],
                      :y [:numerical [77.65 217.35]]},
                     :chart-type :scatter,
                     :margins {:x [0.05 0.05], :y [0.05 0.05]},
                     :shape   (constantly \0) 
                     :position [0 0],
                     :series-id 1}               
                    {:w 600 :h 300 :x sx :y sy :extent {:x [412.2 517.8]  :y [77.65 217.35]}})
      ;;since render-graph returns a clojure2D canvas, we are free to render it.
      :canvas
      (show)))

;;This a a fairly lower-level look at what the plot spec is doing -
;;for only one series, the scatter plot marks defined by the 2 points
;;of input data.

;;Notably, most of the options for customization, e.g. size, color,
;;stroke size, etc. expect functions.  We idiomatically wrap constant
;;values using clojure.core/constantly to conform to this facade.

)


;;So our minimal specification for a scatter plot has some notion of
;;series (one or more), columns (cols), rows, visual extents, and
;;scales.  This is fairly common for any plotting library, but it's
;;nice to have access to it from clojure, and to not have it compiled
;;or similarly munged.

;;Note: as we add more information - e.g.  to project the
;;specification "onto" a canvas - we will necessarily introduce more
;;information that's orthogonal to the essence of plotting, but
;;relevant to the implementation of the rendering backend in
;;clojure2d.

;;So there's another immediate feature - we have a decoupling of
;;specification (data) from effect (rendering).  This is a very nice
;;property.

;; Adding More
;; ===========

;;The current scatter plot is intentionally lame, but still
;;a decent visual representation.  Let's add more to it
;;and construct a more typical plot...

;;Now that we know the difference between specificying and
;;rendering a plot, we will freely intermingle the two
;;for simple visualizations as a coherent pipeline.
;;This conforms to the typical examples from cljplot.
;;Still, we are free to parameterize our specification
;;and build up complex representations or customize
;;the plots directly (e.g. outside the builder tooling).


;;We'll reproduce our plot from before, but this time
;;add some axes to it to provide the viewer with
;;a visual cue for the scale and orientation of
;;the points.
(defn xyplot [pairs & {:keys [width height title]
                              :or {width 600 height 300
                                   title "cljplot charting library"
                                   }}]
  (-> (b/series [:grid] ;;just a layer of gridlines...
                [:scatter pairs] ;;a scatter plot from the xy pairs.
                )
      (b/preprocess-series)
      ;;note we add axes after preprocessing....
      (b/add-axes :bottom)
      (b/add-axes :left) 
      (b/add-label :top title)
      (b/add-label :bottom "x")
      (b/add-label :left "y")
      (r/render-lattice {:width width :height height})
      (show)))


;;Messing With Visual Attributes
;;==============================

;;Now that we have the basis for civilized plotting,
;;what else can we do to provide a decent visual?
;;Let's add some series labels and mess with the font-size
;;of the title.
(defn xyplot [pairs & {:keys [width height title series-name x-label y-label]
                              :or {width 600 height 300
                                   title "cljplot charting library"
                                   series-name "series-1"
                                   }}]
  (-> (b/series [:grid] ;;just a layer of gridlines...
                [:scatter pairs {:color :blue}] ;;a scatter plot from the xy pairs.
                )
      (b/preprocess-series)
      ;;note we add axes after preprocessing....
      (b/add-axes  :bottom)
      (b/add-axes  :left) 
      (b/add-label :top    title {:font "Courier New" :font-size 30})
      (b/add-label :bottom x-label)
      (b/add-label :left   y-label)
      (b/add-legend "Legend"
                    [[:rect series-name {:color :blue}]])
      (r/render-lattice {:width width :height height})
      (show)))

(-> 2500 (random-points 600 300) (xyplot :series-name "blah"))

;;More Control Over Size and Color
;;================================
(defn even-odd-plot [pairs & {:keys [width height title x-label y-label]
                                :or {width 600 height 300
                                     title "cljplot charting library"
                                     series-name "series-1"
                                     }}]
  (let [{:keys [even odd]} (group-by #(if (odd? (first %)) :odd :even) pairs)]
    (-> (b/series [:grid] ;;just a layer of gridlines...
                  [:scatter even  {:color :red :shape :rect}] ;;a scatter plot from the xy pairs.
                  [:scatter odd   {:color :blue :shape \o}]
                  )
        (b/preprocess-series)
        ;;note we add axes after preprocessing....
        (b/add-axes  :bottom)
        (b/add-axes  :left) 
        (b/add-label :top    title {:font "Courier New" :font-size 30})
        (b/add-label :bottom x-label)
        (b/add-label :left   y-label)
        (b/add-legend "Legend"
                      [[:rect "even" {:color :red}]
                       [:circle "odd"  {:color :blue}]
                       ])
        (r/render-lattice {:width width :height height})
        (show))))

;;since series is just a map....we can derive a legend...
(defn derive-legend [series]
  (let [xs (->> series :series vals (filter (fn [s]
                                              (not= (first s) :grid))))]
    (map identity #_(juxt :series-id :color) xs)))

(let [palette (clojure2d.color/random-palette)]
  (-> 2500
      (random-points 600 300)
      (xyplot :series-name "Colorized"
              :title "Multiple Colors"
              :color (fn [[x _] conf]  (rand-nth palette)))))

;;for clarity of reading...
;;I've decomposed some of the low-level graphics
;;and math operations from clojure2d and fastmath,
;;to explain what's going on...
(def scatter-options
  (let [random-double    (fn [] (rnd/drand 0.1 1.0))
        ;;let's randomly scale our points by random-double^3 * 10,
        ;;where random-double is a uniformly distributed number
        ;;from [0.1 ... 1.0]
        xy->random-size  (fn [_ _] ;;[x y] but we don't care
                           (* 10 (m/pow (random-double) 3)))]
  {:size  xy->random-size 
   :color (fn [[x _] conf]
            (let [[mn mx] (get-in conf [:extent :x 1])]
              (c/set-alpha (gradient (m/norm x mn mx)) 50)))})

;;For our scatter plot, let's use our cljplot logo as
;;the source information for the x,y coordinate pairs.
(defn logo->xyplot []
  (let [gradient  (c/gradient-presets :two-heads-filonov)
        side-conf {:color (nth (iterate c/brighten (gradient 0.1)) 3) :area? true :margins {:x [0.02 0.02]}}
        pairs (->> (repeatedly #(v/vec2 (rnd/irand 200) (rnd/irand 100)))
                   (filter #(pos? (c/luma (apply p/get-color logo %))))
                   (map #(v/add % (v/generate-vec2 rnd/grand)))
                   (take 2500))]
    (-> (b/series [:grid]
                  [:scatter pairs {:size (fn [_ _] (* 10 (m/pow (rnd/drand 0.1 1.0) 3)))
                                   :color (fn [[x _] conf]
                                            (let [[mn mx] (get-in conf [:extent :x 1])]
                                              (c/set-alpha (gradient (m/norm x mn mx)) 50)))}])
        (b/preprocess-series)
        (b/add-axes :bottom)
        (b/add-axes :left)
        (b/add-label :bottom "cljplot charting library")
        (r/render-lattice {:width 600 :height 300})
        (save "results/examples/logo.jpg")
        (show))))

;;Note: we could just as easily use "any" function
;;for our coloring scheme.



(let [gradient  (c/gradient-presets :two-heads-filonov)
      side-conf {:color (nth (iterate c/brighten (gradient 0.1)) 3) :area? true :margins {:x [0.02 0.02]}}
      pairs (->> (repeatedly #(v/vec2 (rnd/irand 200) (rnd/irand 100)))
                 (filter #(pos? (c/luma (apply p/get-color logo %))))
                 (map #(v/add % (v/generate-vec2 rnd/grand)))
                 (take 2500))]
  (-> (b/series [:grid]
                [:scatter pairs {:size (fn [_ _] (* 10 (m/pow (rnd/drand 0.1 1.0) 3)))
                                 :color (fn [[x _] conf]
                                          (let [[mn mx] (get-in conf [:extent :x 1])]
                                            (c/set-alpha (gradient (m/norm x mn mx)) 50)))}])
      (b/preprocess-series)
      (b/add-side :top 25 (b/series [:density (map first pairs) side-conf]))
      (b/add-side :right 25 (b/series [:density (map second pairs) side-conf]))
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "cljplot charting library")
      (r/render-lattice {:width 600 :height 300})
      (save "results/examples/logo.jpg")
      (show)))


;;Before solving it, let's deconstruct what's happening with
;;the example...

(let [;; define a color gradient, no idea what the presets are.
      gradient (c/gradient-presets :two-heads-filonov)
      ;;define configuration for our "side" graphs...
      ;;This
      side-conf {:color (nth (iterate c/brighten (gradient 0.1)) 3) :area? true :margins {:x [0.02 0.02]}}
      pairs (->> (repeatedly #(v/vec2 (rnd/irand 200) (rnd/irand 100)))
                 (filter #(pos? (c/luma (apply p/get-color logo %))))
                 (map #(v/add % (v/generate-vec2 rnd/grand)))
                 (take 2500)))
