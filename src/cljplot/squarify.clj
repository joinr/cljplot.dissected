;;Clojure port of squarify.py,
;;original python version
;; Copyright 2013 Uri Laserson

;;    Licensed under the Apache License, Version 2.0 (the "License");
;;    you may not use this file except in compliance with the License.
;;    You may obtain a copy of the License at

;;        http://www.apache.org/licenses/LICENSE-2.0

;;    Unless required by applicable law or agreed to in writing, software
;;    distributed under the License is distributed on an "AS IS" BASIS,
;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;    See the License for the specific language governing permissions and
;; limitations under the License.

(ns cljplot.squarify)
;; # Squarified Treemap Layout
;; # Implements algorithm from Bruls, Huizing, van Wijk, "Squarified Treemaps"
;; #   (but not using their pseudocode)


;; # INTERNAL FUNCTIONS not meant to be used by the user

;; def pad_rectangle(rect):
;;     if rect["dx"] > 2:
;;         rect["x"] += 1
;;         rect["dx"] -= 2
;;     if rect["dy"] > 2:
;;         rect["y"] += 1
;;         rect["dy"] -= 2

(defn  pad-rectangle [{:keys [dx x dy y] :as r}]
  (as-> r rect
    (if (> dx 2)
      (update rect :x inc)
      (update rect :dx - 2))
    (if (> dy 2)
      (update rect :y inc)
      (update rect :dy - 2))))
      
;; def layoutrow(sizes, x, y, dx, dy):
;;     # generate rects for each size in sizes
;;     # dx >= dy
;;     # they will fill up height dy, and width will be determined by their area
;;     # sizes should be pre-normalized wrt dx * dy (i.e., they should be same units)
;;     covered_area = sum(sizes)
;;     width = covered_area / dy
;;     rects = []
;;     for size in sizes:
;;         rects.append({"x": x, "y": y, "dx": width, "dy": size / width})
;;         y += size / width
;;     return rects

(defn layout-row [sizes x y dx dy]
  (let [covered-area (reduce + 0.0 sizes)
        width        (/ covered-area dy)]
    (->> sizes
         (reduce (fn [[y acc] size]
                   [(+ y (/ size width))
                    (conj acc {:x x, :y y, :dx width, :dy (/ size width)})])
                 [y []])
         second)))

;; def layoutcol(sizes, x, y, dx, dy):
;;     # generate rects for each size in sizes
;;     # dx < dy
;;     # they will fill up width dx, and height will be determined by their area
;;     # sizes should be pre-normalized wrt dx * dy (i.e., they should be same units)
;;     covered_area = sum(sizes)
;;     height = covered_area / dx
;;     rects = []
;;     for size in sizes:
;;         rects.append({"x": x, "y": y, "dx": size / height, "dy": height})
;;         x += size / height
;;     return rects

(defn layout-col [sizes x y dx dy]
  (let [covered-area (reduce + 0.0 sizes)
        height       (/ covered-area dx)]
    (->> sizes 
         (reduce (fn [[x acc] size]
                   [(+ x (/ size height))
                    (conj acc {:x x, :y y, :dx (/ size height), :dy height})])
                 [x []])
         second)))

;; def layout(sizes, x, y, dx, dy):
;;     return (
;;         layoutrow(sizes, x, y, dx, dy) if dx >= dy else layoutcol(sizes, x, y, dx, dy)
;;     )

(defn layout [sizes x y dx dy]
  ((if (>= dx dy)layout-row layout-col)
   sizes x y dx dy))


;; def leftoverrow(sizes, x, y, dx, dy):
;;     # compute remaining area when dx >= dy
;;     covered_area = sum(sizes)
;;     width = covered_area / dy
;;     leftover_x = x + width
;;     leftover_y = y
;;     leftover_dx = dx - width
;;     leftover_dy = dy
;;     return (leftover_x, leftover_y, leftover_dx, leftover_dy)

(defn leftover-row [sizes x y dx dy]
  (let [covered-area (reduce + 0.0 sizes)
        width (/ covered-area dy)
        lx  (+ x width)
        ly  y
        ldx (- dx width)
        ldy dy]
    [lx ly ldx ldy]))


;; def leftovercol(sizes, x, y, dx, dy):
;;     # compute remaining area when dx >= dy
;;     covered_area = sum(sizes)
;;     height = covered_area / dx
;;     leftover_x = x
;;     leftover_y = y + height
;;     leftover_dx = dx
;;     leftover_dy = dy - height
;;     return (leftover_x, leftover_y, leftover_dx, leftover_dy)

(defn leftover-col [sizes x y dx dy]
  (let [covered-area (reduce + 0.0 sizes)
        height (/ covered-area dx)
        lx x
        ly (+ y height)
        ldx dx
        ldy (- dy height)]
    [lx ly ldx ldy]))

;; def leftover(sizes, x, y, dx, dy):
;;     return (
;;         leftoverrow(sizes, x, y, dx, dy)
;;         if dx >= dy
;;         else leftovercol(sizes, x, y, dx, dy)
;;     )

(defn leftover [sizes x y dx dy]
  ((if (>= dx dy) leftover-row leftover-col)
   sizes x y dx dy))

;; def worst_ratio(sizes, x, y, dx, dy):
;;     return max(
;;         [
;;             max(rect["dx"] / rect["dy"], rect["dy"] / rect["dx"])
;;             for rect in layout(sizes, x, y, dx, dy)
;;         ]
;;     )

(defn worst [{:keys [dx dy]}]
             (max (/ dx dy)
                  (/ dy dx)))

(defn worst-ratio [sizes x y dx dy]
  (reduce max (map worst (layout sizes x y dx dy))))
    

;; # PUBLIC API
(defn find-split [sizes x y dx dy]
  (let [bound (count sizes)]
    (loop [idx 1]
      (if (and (< idx bound)
               (>= (worst-ratio (take idx sizes)      x y dx dy)
                   (worst-ratio (take (inc idx) sizes) x y dx dy)))
        (recur (inc idx))
        idx))))

(defn squarify
  "Compute treemap rectangles.

    Given a set of values, computes a treemap layout in the specified geometry
    using an algorithm based on Bruls, Huizing, van Wijk, 'Squarified Treemaps'.
    See README for example usage.

    Parameters
    ----------
    sizes : list-like of numeric values
        The set of values to compute a treemap for. `sizes` must be positive
        values sorted in descending order and they should be normalized to the
        total area (i.e., `dx * dy == sum(sizes)`)
    x, y : numeric
        The coordinates of the 'origin'.
    dx, dy : numeric
        The full width (`dx`) and height (`dy`) of the treemap.

    Returns
    -------
    list[dict]
        Each dict in the returned list represents a single rectangle in the
        treemap. The order corresponds to the input order.
    "

  [sizes, x, y, dx, dy]
  (let [sizes (map double sizes)]
    (cond
      (not (seq sizes)) []          
      (== (count sizes) 1) (layout sizes x y dx dy)
      :else
      (let [idx       (find-split sizes x y dx dy)
            [current remaining] [(take idx sizes) (drop idx sizes)]
            [lx ly ldx ldy :as left] (leftover current x y dx dy)
            ]
        (concat (layout current x y dx dy)
                (squarify remaining lx ly ldx ldy))))))

(defn padded-squarify
  "Compute padded treemap rectangles.
    See `squarify` docstring for details. The only difference is that the
    returned rectangles have been 'padded' to allow for a visible border."
  [sizes x y dx dy]
  (->> (squarify sizes x y dx dy)
       (map pad-rectangle)))


;; def normalize_sizes(sizes, dx, dy):
;;     """Normalize list of values.

;;     Normalizes a list of numeric values so that `sum(sizes) == dx * dy`.

;;     Parameters
;;     ----------
;;     sizes : list-like of numeric values
;;         Input list of numeric values to normalize.
;;     dx, dy : numeric
;;         The dimensions of the full rectangle to normalize total values to.

;;     Returns
;;     -------
;;     list[numeric]
;;         The normalized values.
;;     """
;;     total_size = sum(sizes)
;;     total_area = dx * dy
;;     sizes = map(float, sizes)
;;     sizes = map(lambda size: size * total_area / total_size, sizes)
;;     return list(sizes)

(defn normalize-sizes [sizes dx dy]
  (let [total-size (reduce + 0.0 sizes)
        total-area (* dx dy)]
    (->> sizes
         (map (fn [x] (/ (* (double x) total-area) total-size)))
         vec)))


(defn stest []
  (let [x 0.0
        y 0.0
        width 700.0
        height 433.0
        values  [500, 433, 78, 25, 25, 7]
        ;values (sort  values)
        values (normalize-sizes values width height)
        rects  (squarify values, x, y, width, height)
        padded-rects nil #_(padded-squarify values x y width height)]
    (with-meta rects {:padded padded-rects})))

(def expected
  [{
    :dy 433,
    :dx 327.7153558052434,
    :x 0,
    :y 0
  },
  {
    :dy 330.0862676056338,
    :dx 372.2846441947566,
    :x 327.7153558052434,
    :y 0
  },
  {
    :dy 102.9137323943662,
    :dx 215.0977944236371,
    :x 327.7153558052434,
    :y 330.0862676056338
  },
  {
    :dy 102.9137323943662,
    :dx 68.94160077680677,
    :x 542.8131502288805,
    :y 330.0862676056338
  },
  {
    :dy 80.40135343309854,
    :dx 88.24524899431273,
    :x 611.7547510056874,
    :y 330.0862676056338
  },
  {
    :dy 22.51237896126767,
    :dx 88.2452489943124,
    :x 611.7547510056874,
    :y 410.4876210387323
   }])

(def expected (for [{:keys [x y dx dy]} expected]
                {:x x :y y :dx dx :dy dy}))


(defn tree-map [sizes & {:keys [norm-x norm-y color label value ax pad bar text]}]
  )

;; def plot(
;;     sizes,
;;     norm_x=100,
;;     norm_y=100,
;;     color=None,
;;     label=None,
;;     value=None,
;;     ax=None,
;;     pad=False,
;;     bar_kwargs=None,
;;     text_kwargs=None,
;;     **kwargs
;; ):
;;     """Plotting with Matplotlib.

;;     Parameters
;;     ----------
;;     sizes
;;         input for squarify
;;     norm_x, norm_y
;;         x and y values for normalization
;;     color
;;         color string or list-like (see Matplotlib documentation for details)
;;     label
;;         list-like used as label text
;;     value
;;         list-like used as value text (in most cases identical with sizes argument)
;;     ax
;;         Matplotlib Axes instance
;;     pad
;;         draw rectangles with a small gap between them
;;     label
;;         fontsize of the labels
;;     bar_kwargs : dict
;;         keyword arguments passed to matplotlib.Axes.bar
;;     text_kwargs : dict
;;         keyword arguments passed to matplotlib.Axes.text
;;     **kwargs
;;         Any additional kwargs are merged into `bar_kwargs`. Explicitly provided
;;         kwargs here will take precedence.

;;     Returns
;;     -------
;;     matplotlib.axes.Axes
;;         Matplotlib Axes
;;     """

;;     import matplotlib.pyplot as plt

;;     if ax is None:
;;         ax = plt.gca()

;;     if color is None:
;;         import matplotlib.cm
;;         import random

;;         cmap = matplotlib.cm.get_cmap()
;;         color = [cmap(random.random()) for i in range(len(sizes))]

;;     if bar_kwargs is None:
;;         bar_kwargs = {}
;;     if text_kwargs is None:
;;         text_kwargs = {}
;;     if len(kwargs) > 0:
;;         bar_kwargs.update(kwargs)

;;     normed = normalize_sizes(sizes, norm_x, norm_y)

;;     if pad:
;;         rects = padded_squarify(normed, 0, 0, norm_x, norm_y)
;;     else:
;;         rects = squarify(normed, 0, 0, norm_x, norm_y)

;;     x = [rect["x"] for rect in rects]
;;     y = [rect["y"] for rect in rects]
;;     dx = [rect["dx"] for rect in rects]
;;     dy = [rect["dy"] for rect in rects]

;;     ax.bar(
;;         x, dy, width=dx, bottom=y, color=color, label=label, align="edge", **bar_kwargs
;;     )

;;     if not value is None:
;;         va = "center" if label is None else "top"

;;         for v, r in zip(value, rects):
;;             x, y, dx, dy = r["x"], r["y"], r["dx"], r["dy"]
;;             ax.text(x + dx / 2, y + dy / 2, v, va=va, ha="center", **text_kwargs)

;;     if not label is None:
;;         va = "center" if value is None else "bottom"
;;         for l, r in zip(label, rects):
;;             x, y, dx, dy = r["x"], r["y"], r["dx"], r["dy"]
;;             ax.text(x + dx / 2, y + dy / 2, l, va=va, ha="center", **text_kwargs)

;;     ax.set_xlim(0, norm_x)
;;     ax.set_ylim(0, norm_y)

;;     return ax
