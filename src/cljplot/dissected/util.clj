;;A collection of utility functions that
;;emerged during exploration of the existing
;;libraries.  Some of these may be candidates
;;or prototypes for inclusion as cljplot features.
(ns cljplot.dissected.util
  (:require [cljplot.build]))

;;Adding legends manually is fine, but we'd like to just
;;have the program do it for us.

;;since series is just a map....we can derive a legend...
;;Even though we don't have a corresponding auto-legend
;;function in cljplot.build currently, we can trivially
;;add one since the series definitions are built
;;on maps.  The legend specification conforms to
;;cljplot.axis
(defn derive-legend [series]
  (for [[t d config] (->> series
                          :series
                          vals
                          first
                          (filter (fn [s]
                                    (not= (first s) :grid))))]
    (let [{:keys [shape color series-id series-label]} config
          shape (shape 0 0)]
      [:shape (or series-label series-id)  {:color (color 0 0) :shape shape}])))

;;user can submit nil for the name, and we won't
;;derive a legend.  Otherwise, "" will provide a
;;legend with no label.
(defn add-derived-legend [series name]
  (b/add-legend series name (derive-legend series)))
             
  ;;this is a convenience function to give us a default
  ;;axes labels, title, and legend...
(defn add-basic-styling [series & {:keys [title x-label y-label legend? legend-label]
                                   :or {title "cljplot"
                                        x-label "x"
                                        y-label "y"
                                        legend? true
                                        legend-label "Legend"}}]
  (-> series
      (b/add-axes  :bottom)
      (b/add-axes  :left) 
      (b/add-label :top    title {:font "Courier New" :font-size 30})
      (b/add-label :bottom x-label)
      (b/add-label :left   y-label)
      (as-> series (if legend?
                     (add-derived-legend  series legend-label)
                     series))))
