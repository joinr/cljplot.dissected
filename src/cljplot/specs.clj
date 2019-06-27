(ns cljplot.specs
  (:require [clojure.spec.alpha :as s]
            [spec-provider.provider :as sp]))

(s/def ::margin           (s/or :double double? :integer integer?))
(s/def ::colorspace       keyword?)
(s/def ::grid-type        keyword?)
(s/def ::blur-kernel-size double?)
(s/def ::kernel-bandwidth any?)
(s/def ::step             double?)
(s/def ::fill?            boolean?)
(s/def ::padding-out      double?)
(s/def ::logarithmic?     boolean?)
(s/def ::cells            integer?)
(s/def ::extent-type      keyword?)
(s/def ::percents?        boolean?)
(s/def ::domain           (s/coll-of integer?))
(s/def ::dash-phase       double?)
(s/def ::dash             (s/coll-of double?))
(s/def ::cap              keyword?)
(s/def ::size             (s/or :double double? :integer integer?))
(s/def ::stroke
  (s/keys :req-un [::size] :opt-un [::cap ::color ::dash ::dash-phase]))
(s/def ::color
  (s/nilable
   (s/or
    :collection
    ::color-bar
    :simple
    (s/or :any any? :keyword keyword?))))
(s/def ::x
 (s/or
  :collection
  (s/coll-of #{0.1 0.05})
  :map  (s/keys :req-un [::color ::stroke])))

(s/def ::annotate-fmt any?)
(s/def ::type         (s/nilable keyword?))
(s/def ::point        (s/keys :req-un [::size ::type]))
(s/def ::padding-in   double?)
(s/def ::generator    keyword?)
(s/def ::kernel       keyword?)
(s/def ::length       integer?)
(s/def ::gap          integer?)
(s/def ::shape        any?)
(s/def ::gradient    any?)

(s/def ::y
 (s/or  :collection  ::color-bar
        :map  (s/keys :req-un [::color ::stroke])))

(s/def ::margins (s/keys :req-un [::x] :opt-un [::y]))

(s/def ::padding double?)

(s/def ::contours integer?)

(s/def ::wrap? boolean?)

(s/def ::wrap-method (s/nilable keyword?))

(s/def ::normalize? boolean?)

(s/def ::font string?)

(s/def ::smooth? boolean?)

(s/def ::stroke? boolean?)

(s/def ::points integer?)

(s/def ::size-range (s/coll-of ::size))

(s/def ::area? boolean?)

(s/def ::kernel-params any?)

(s/def ::bins any?)

(s/def ::palette
  (s/coll-of (s/or :collection ::color-bar :simple #{:white :black})))

(s/def ::color-bar (s/coll-of double?))

(s/def ::grid keyword?)

(s/def ::method keyword?)

(s/def ::scale double?)

(s/def ::font-size integer?)

(s/def ::outliers? boolean?)

(s/def ::size-bar double?)

(s/def ::alpha-factor double?)

(s/def ::jitter double?)

(s/def ::annotate? boolean?)

(s/def ::interpolation any?)

(s/def ::scale-z (s/coll-of (s/or :double double? :keyword keyword?)))

(s/def ::marker-size integer?)

(s/def ::distort double?)

(s/def ::samples any?)

(s/def ::font-style keyword?)

(s/def ::permutation integer?)







;;Missing / useful specs....


;;::pairs

;;::scale-map

;;::scale

;;::plot-type

;;::plot-config

;;::chart-data


;;::mark
(def mark-shapes #{\* \} \{ \A \V \v \> \< \^ \x
                   \/ \\ \+ \- \| \s \S \o \O \. })

(s/def ::mark-shape mark-shapes) 


;;(def known-charts (set (keys (methods cljplot.common/render-graph))))

(def known-series
  #{:pacf :stack-horizontal :gbubble :qqplot :rbar :histogram :heatmap :normal-plot
    :default :free :bar :density-strip :grid :cdf :vline :cloud :complex :trace :ppplot
    :box :sbar :bubble :field :hline :scatter :vector :function :lollipop :sarea :scalar
    :line :label :stack :violin :area :strip :lag :axis :extent-stat :density :abline
    :stack-vertical :density-2d :acf :rug})

(s/def ::series-type known-charts)
