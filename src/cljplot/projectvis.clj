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
            [spork.util.codecount :as cc]
            ))

(def res (cc/classify-projects "../spork/" "../marathon/"))
;;#'codecount/res
res
;;({:path "../spork/",
;;  :totals {:code 38375, :comment 13251, :blank 7046}}
;; {:path "../marathon/",
;;  :totals {:code 64273, :comment 25675, :blank 11720}})

(def totals (reduce (partial merge-with +) (map :totals res)))

(defn dataset [ps]
  (for [p ps
        [k {:keys [comment code blank] :as stats}] (-> p meta :pieces)]
    (assoc stats :path k :series (:path p))))

(defn ns-children [x]
  (some-> x meta :pieces))

(defn data-tree [root]
  (tree-seq #(or (ns-children %) (coll? %)) #(map (fn [x] (or (seq (ns-children x)) x)) %) root))

;;styling either by a 
(defn with-styling [m & xs]
  (for [[idx x] (map-indexed vector xs)]
    (let [style (nth x 2 {:series-id idx})
          new-style (or (some-> style :series-label m)
                         (m idx))]
      (assoc x 2 (merge style (or (some-> style :series-label m)
                                  new-style))))))

(let [p       (c/palette-presets :tableau-10-2)
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


(let [p       (c/palette-presets :tableau-10-2)
      styling [{:series-label "blue"   :color (p 0) :shape \o}
               {:series-label "orange" :color (p 1)}]]
  (-> (b/series [:grid]
                [:tree-map (mapv (fn [x] [x (rand-int 50) (rand-int 100) (rand-int 100)])  (range 100))])
      (b/preprocess-series)
      ;(defaults/add-basic-styling)
      (r/render-lattice {:with 1080 :height 720})
      (show)))
