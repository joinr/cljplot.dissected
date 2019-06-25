;;useful defaults for legends and styling and the like.
(ns cljplot.defaults
  (:require [cljplot.build :as b]))

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


;;While we're at it, we keep repeating the same styling and
;;chart defaults.  Let's encapsulate that in a basic function.

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
