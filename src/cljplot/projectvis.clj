(ns cljplot.projectvis 
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
            [fastmath.kernel :as kk]
            ;;monkey patches to get this working, pending issues in repo.
            [cljplot.defaults :as defaults]
            [cljplot.impl.treemap]
            [cljplot.squarify :as squarify]
            [spork.util.codecount :as cc]
            ))


(defn path->key [p]
  (->> (spork.util.io/list-path p)
       (into [] (comp (drop 1)
                      (map #(symbol (clojure.string/replace % ".clj" "")))))))
(defn dataset [ps]
  (->>  (for [p ps
              [k {:keys [path comment code blank] :as stats}] (-> p meta :pieces)
              :when (clojure.string/includes? k "src")
              ]
          (let [s (path->key k)]
            (assoc stats :path s :series (first s))))
        (sort-by :path)))

(defn datatree
  ([ds depth]
   (if (map? ds) ds
       (let [[current deeper]
             (reduce (fn [[c d] r]
                       (if (> (-> r :path count) (inc depth))
                         [c (conj d r)]
                         [(conj c r) d]))
                     [[] []] ds)]
         (into current
               (for [[g xs] (group-by #(-> % :path (nth depth)) deeper)]
                 (let [stats (reduce (fn [l r]
                                       {:comment (+ (:comment l 0) (:comment r 0))
                                        :code    (+ (:code l 0) (:code r 0))
                                        :blank   (+ (:blank l 0) (:blank r 0))})
                                     {:comment 0 :code 0 :blank 0}
                                     xs)] 
                   {:path g
                    :stats stats
                    :children (datatree xs (inc depth))}))))))
  ([ds] (datatree ds 0)))

(defn tree->rects [root]
  (map (fn [r]
         (as-> {:label (if (coll? (:path r))
                         (last (:path r))
                         (:path r))
                           
                :area  (or (:stats r)
                           (select-keys r [:code :comment :blank]))} m
              (if (and (:children r)
                       (> (count (:children r)) 1))
                (assoc m :children (tree->rects (:children r)))
                m))
         ) root))

(defn layout-tree [root x y width height & {:keys [node->area depth]
                                            :or {node->area (fn line-count [r]
                                                              (reduce +  (vals (:area r))))
                                                 depth 0}}]
  (let [sorted (sort-by (comp - node->area) (filter (comp pos? node->area) root))
        sizes  (squarify/normalize-sizes (map node->area sorted) width height)
        rects  (squarify/squarify sizes x y width height)]
    (map (fn [r {:keys [x y dx dy] :as rect}]
           (if (:children r)
             (assoc rect
                    :depth depth
                    :label (:label r)
                    :children (layout-tree (:children r) x y dx dy
                                           :node->area node->area
                                           :depth (inc depth)))
             (assoc rect :depth depth :label (:label r))))
         sorted rects)))

(defn layout->rects [xs]
  (->> (tree-seq (fn [x]
                   (cond (map? x) (:children x)
                         :else (coll? x)))
                 (fn [r]
                   (cond (map? r)   (:children r)
                         (coll? r) r)) xs)
       flatten
       (map #(dissoc % :children))
       distinct
       (sort-by :depth)))

(defn classified->boxes
  "given a project classification p, a root x y, and
   a containing box, w h, layouts out the project dependencies
   returning a seq of labeled rectangles by depth
   suitably for rendering."
  ([p x y w h]
   (->>  (-> p
             dataset
             datatree
             tree->rects
             (layout-tree x y w h)
             layout->rects)
         (map (fn [{:keys [dx dy] :as r}]
                (assoc r :w dx :h dy)))))
  ([p x y] (let [area (reduce + (mapcat vals (map :totals p)))
                 half (/ area 2.0)]
             (if (even? area)
               (classified->boxes p 0 0 half half)
               (classified->boxes p 0 0 (long half) (inc (long half))))))
  ([p] (classified->boxes p 0 0)))


(comment
;;scripting bit
;;Aim our thing at a couple of root folders with
;;typical lein layout (e.g. src/blah).
(def res (cc/classify-projects "../spork/" "../marathon/"))

;;#'codecount/res
res
;;({:path "../spork/",
;;  :totals {:code 38375, :comment 13251, :blank 7046}}
;; {:path "../marathon/",
;;  :totals {:code 64273, :comment 25675, :blank 11720}})

(def totals (reduce (partial merge-with +) (map :totals res)))
;;convenient totals stats...

;;and we get back a result mapping
;;some code stats to the path.

;;The metadata hides a :pieces map,
;;which is a collection of similar statistics
;;for all the source files traversed.

;;So we have the stuff we need to build a
;;tree to map visually.

;;we can use our pipeline to get
;;cljplot friendly "boxes" of {x y w h label}
;;that are laid out according to the
;;squarify algorithm.
(def rs (classified->boxes res))

;;then we can visualize said boxes
;;in various naive ways.  This is
;;our first tree-map!
(-> (b/series [:grid]
              [:boxes rs {:color :white}])
    (b/preprocess-series)
    (b/add-axes  :bottom)
    (b/add-axes  :left) 
    (r/render-lattice {:width 1000 :height 1000})
    (save "primitivetree.png")
    (show))
)

(comment
  ;;Another simpler example, this time with cljplot itself.
(-> (b/series [:grid]
              [:boxes (-> (cc/classify-projects "../cljplot/" "../fastmath/")
                          classified->boxes) {:color :blue :outline :white}])
    (b/preprocess-series)
    (b/add-axes  :bottom)
    (b/add-axes  :left  )  
    (r/render-lattice {:width 1000 :height 1000})
    (save "cljplot.png")
    (show))
)





;;Back Burner
;;===========





;;styling either by a 
#_(defn with-styling [m & xs]
  (for [[idx x] (map-indexed vector xs)]
    (let [style (nth x 2 {:series-id idx})
          new-style (or (some-> style :series-label m)
                         (m idx))]
      (assoc x 2 (merge style (or (some-> style :series-label m)
                                  new-style))))))

#_(let [p       (c/palette-presets :tableau-10-2)
      styling [{:series-label "blue"   :color (p 0) :shape \o}
               {:series-label "orange" :color (p 1)}]]
  (-> (b/series [:grid])
      (into (with-styling styling
              [:scatter (mapv (fn [x] [x (rand-int 600)]) (range 1000))]
              [:scatter (mapv (fn [x] [x (rand-int 50)])  (range 1000))]))
      (b/preprocess-series)
      (defaults/add-basic-styling)
      (r/render-lattice {:with 1080 :height 720})
      (show)))

#_(-> (b/series [:grid]
              [:scatter (relative-rect 0 0 613.284433577832 1000.0) {:color :red}]
              [:scatter (relative-rect 0 0 386.71556642216797 999.9999999999998) {:color :blue}])
                       
    (b/preprocess-series)
    (defaults/add-basic-styling)
    (r/render-lattice {:with 1000 :height 1000})
    (show))

;;playing with treemaps

#_(-> (b/series [:grid]
              [:boxes [[0 0 100 100] [500 500 100 100]]])
      (b/preprocess-series)
      (b/add-axes  :bottom)
      (b/add-axes  :left) 
      (r/render-lattice {:width 1000 :height 1000})
      (show))


#_(-> (b/series [:grid]
               [:scatter (repeatedly 1000 (fn [] [(rand-int 100) (rand-int 100)]))
                {:color :red}]
               [:scatter (repeatedly 1000 (fn [] [(+ 100 (rand-int 100)) (+ 100 (rand-int 100))]))
                {:color :blue}])
     (b/preprocess-series)
     (b/add-axes  :bottom)
     (b/add-axes  :left)
     (r/render-lattice {:width 1000 :height 1000})
     (show))
