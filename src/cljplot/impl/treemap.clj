;;simple tree-map implementation.
(ns cljplot.impl.treemap
  (:require [clojure2d.core :refer :all]
            [cljplot.common :refer :all]
            [cljplot.scale :as s]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;;we'd like to render a node.
;;and its children can be a different color?


;;we're really just rendering rectangles at the end.
;;working backwards from there..
(defmethod render-graph :tree-map [_ data {:keys [color stroke size shape] :as conf}
                                   {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)]
    (do-graph chart-data :highest 
      (let [coords (mapv (fn [[x y w h]]
                           (into (transform c (scale-x x) (scale-y y)) [w h])) data)]
        (reset-matrix c) ;;c is an implicit arg for the canvas
        (doseq [[v [tx ty w h]] (map vector data coords)
                ;:let [local-stroke (update stroke :size (:size stroke) v conf)]
                ]
          (filled-with-stroke c :white :black rect tx ty w h)
          )))))
