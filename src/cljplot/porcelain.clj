;;WIP port of incanter.charts...
(ns cljplot.dissected.porcelain)

(defprotocol IDataSeq
  (as-dataseq [o]))

(defn dataseq [xs]
  (cond
    (extends? IDataSeq (type xs))
      (as-dataseq xs)
    (seqable? xs) xs
    (throw (ex-info "unknown data sequence, expected IDataSeq or Seqable"
                    {:type (type xs)}))))

;;tbd...
(defn scatter-plot*
  ([x y & options]
    (let [opts (when options (apply assoc {} options))
          data (or (:data opts) $data)
          _x   (dataseq x data)
          _y   (dataseq y data)
          _group-by (when (:group-by opts)
                      (dataseq (:group-by opts) data))
          x-groups (when _group-by
                     (map #($ 0 %)
                          (vals ($group-by 1 (conj-cols _x _group-by)))))
          y-groups (when _group-by
                     (map #($ 0 %)
                          (vals ($group-by 1 (conj-cols _y _group-by)))))
          __x (in-coll (if x-groups (first x-groups) _x))
          __y (in-coll (if y-groups (first y-groups) _y))
          title (or (:title opts) "")
          x-lab (or (:x-label opts) (str 'x))
          y-lab (or (:y-label opts) (str 'y))
          series-lab (or (:series-label opts)
                         (if x-groups
                           (format "%s, %s (0)" 'x 'y)
                           (format "%s, %s" 'x 'y)))
          theme (or (:theme opts) :default)
          legend? (true? (:legend opts))
          data-series (XYSeries. series-lab)
          _dataset (XYSeriesCollection.)
          chart (do
                  (dorun
                   (map (fn [x y]
                          (if (and (not (nil? x)) (not (nil? y)))
                            (.add data-series (double x) (double y))))
                        __x __y))
                  (.addSeries _dataset data-series)
                  (org.jfree.chart.ChartFactory/createScatterPlot
                   title
                   x-lab
                   y-lab
                   _dataset
                   org.jfree.chart.plot.PlotOrientation/VERTICAL
                   legend?
                   true            	; tooltips
                   false))
          _ (when x-groups
              (doseq [i (range 1 (count x-groups))]
                (add-points chart
                            (nth x-groups i)
                            (nth y-groups i)
                            :series-label (format "%s, %s (%s)" 'x 'y i))))]
      (.setSeriesShape (-> chart .getPlot .getRenderer) 0 (java.awt.geom.Ellipse2D$Double. -3 -3 6 6))
      (.setSeriesShape (-> chart .getPlot .getRenderer) 1 (java.awt.geom.Rectangle2D$Double. -3 -3 6 6))
      (set-theme chart theme)
      chart)))


(defmacro scatter-plot
  "
  Returns a JFreeChart object representing a scatter-plot of the given data.
  Use the 'view' function to display the chart, or the 'save' function
  to write it to a file.

  Options:
    :title (default '') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)
    :group-by (default nil) -- a vector of values used to group the x and y values into series.
    :density? (default false) -- chart will represent density instead of frequency.
    :nbins (default 10) -- number of bins (i.e. bars)
    :gradient? (default false) -- use gradient on bars

  See also:
    view, save, add-points, add-lines

  Examples:

    (use '(incanter core stats charts datasets))
    ;; create some data
    (def mvn-samp (sample-mvn 1000 :mean [7 5] :sigma (matrix [[2 1.5] [1.5 3]])))

    ;; create scatter-plot of points
    (def mvn-plot (scatter-plot (sel mvn-samp :cols 0) (sel mvn-samp :cols 1)))
    (view mvn-plot)

    ;; add regression line to scatter plot
    (def x (sel mvn-samp :cols 0))
    (def y (sel mvn-samp :cols 1))
    (def lm (linear-model y x))
    (add-lines mvn-plot x (:fitted lm))

    ;; use :group-by option
    (use '(incanter core stats datasets charts))
    ;; load the :iris dataset
    (def iris (get-dataset :iris))
    ;; plot the first two columns grouped by the fifth column
    (view (scatter-plot ($ :Sepal.Width iris) ($ :Sepal.Length iris) :group-by ($ :Species iris)))

    (view (scatter-plot :Sepal.Length :Sepal.Width :data (get-dataset :iris)))

    (view (scatter-plot :Sepal.Length :Sepal.Width :group-by :Species :data (get-dataset :iris)))

    (with-data (get-dataset :iris)
       (view (scatter-plot :Sepal.Length :Sepal.Width)))

    (with-data (get-dataset :iris)
       (view (scatter-plot :Sepal.Length :Sepal.Width :group-by :Species)))



  References:
    http://www.jfree.org/jfreechart/api/javadoc/
    http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html

  "
  ([]
     `(scatter-plot [] [] :x-label "x" :y-label "y"))
  ([x y & options]
    `(let [opts# ~(when options (apply assoc {} options))
           group-by# (:group-by opts#)
           title# (or (:title opts#) "")
           x-lab# (or (:x-label opts#) (str '~x))
           y-lab# (or (:y-label opts#) (str '~y))
           series-lab# (or (:series-label opts#) (if group-by#
                                                   (format "%s, %s (0)" '~x '~y)
                                                   (format "%s, %s" '~x '~y)))
           args# (concat [~x ~y] (apply concat (seq (apply assoc opts#
                                                           [:group-by group-by#
                                                            :title title#
                                                            :x-label x-lab#
                                                            :y-label y-lab#
                                                            :series-label series-lab#]))))]
        (apply scatter-plot* args#))))
