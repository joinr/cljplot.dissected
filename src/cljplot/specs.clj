(ns cljplot.dissected.specs
  (:require [clojure.spec.alpha :as s]))

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
