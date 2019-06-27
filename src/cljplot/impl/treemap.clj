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

(defprotocol IBox
  (as-box [o] "returns a map of {x y w h label}"))

(extend-protocol IBox
  clojure.lang.PersistentArrayMap
  (as-box [o] o)
  clojure.lang.PersistentHashMap
  (as-box [o] o)
  clojure.lang.PersistentVector
  (as-box [o]
    (case (count o)
      4 (zipmap [:x :y :w :h] o)
      5 (zipmap [:x :y :w :h :label] o)
      (throw (ex-info "expected [x y w h label?] for box description" {:input o}))))
  clojure.lang.ISeq
  (as-box [o]
    (as-box (vec o))))

;;we'd like to render a node.
;;and its children can be a different color?

(defn box-extents [xs]
  (let [[xmin xmax ymin ymax]
        (reduce (fn [[xmin xmax ymin ymax] {:keys [x y w h]}]
                  [(min xmin x (+ x w))
                   (max xmax x (+ x w))
                   (min ymin y (+ y h))
                   (max ymax y (+ y h))])
                [0 0 0 0]
                (map as-box xs))]
    {:x [:numerical [xmin xmax]]
     :y [:numerical [ymin ymax]]}))
  

(defmethod data-extent :boxes [_ data _]
  (box-extents data))


#_(defmethod render-graph :label [_ {:keys [s pos shift-y]} {:keys [font font-size font-style color]} {:keys [^int w ^int h orientation] :as chart-data}]
  (let [fix-orientation (assoc chart-data :orientation (case orientation
                                                         :left :right
                                                         :top :bottom
                                                         orientation))]
    (do-graph fix-orientation false
      (when font (set-font c font))
      (when font-size
        (if font-style
          (set-font-attributes c font-size font-style)
          (set-font-attributes c font-size)))

      (when (= orientation :right)
        (-> c
            (translate (/ w 2) (/ h 2))
            (rotate m/PI)
            (translate (- (/ w 2)) (- (/ h 2)))))
      (-> c
          (translate (/ w 2) shift-y) 
          (translate pos)
          (set-color color)
          (text s 0 0 :center)))))

;;this is brittle, and only works for top orientation.
(defn draw-label [c {:keys [s pos shift-y]} {:keys [font font-size font-style color]}
                    {:keys [^int w ^int h orientation] :as chart-data}]
  (when font (set-font c font))
  (when font-size
    (if font-style
      (set-font-attributes c font-size font-style)
      (set-font-attributes c font-size)))
  (case orientation
    :right    (-> c
                  (translate (/ w 2) (/ h 2))
                  
                  (translate (- (/ w 2)) (- (/ h 2))))
    :top      (-> c
                  (translate (/ w 2) shift-y) 
                  (translate pos))
    (-> c
        (translate (/ w 2) shift-y) 
        (translate pos)
        (scale 1.0 -1.0)))
    
  (-> c
      (set-color color)
      (text s 0 0 :center)))

(defn depth-box [scale-x scale-y {:keys [x y w h label depth]}]
  {:x (scale-x x) :y (scale-y y)
   :w (scale-x w) :h (scale-y  h)
   :label label
   :depth (or depth 0.0)})

;;we're really just rendering rectangles at the end.
;;working backwards from there..
;;note: we can trivially extend this to be an arbitary renderer.
(defmethod render-graph :boxes [_ data {:keys [color stroke size shape outline labels] :as conf}
                                   {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        coords  (->> data
                     (map (comp #(depth-box scale-x scale-y %) as-box))
                     (sort-by :depth))
        color       (or color :white)
        ;;compute the color depth as we go.        
        depth-color  (memoize
                      (fn [d]
                        (-> (iterate clojure2d.color/darken color)
                            (nth d))))  
        outline     (or outline :black)
        {:keys [font-size font-color depth-scale]
         :or {font-size 30.0
              font-color :black
              depth-scale 0.8}} labels
        labels? (seq labels)]
  (do-graph chart-data false 
    (doseq [{:keys [x y w h label depth]}  coords]
      (filled-with-stroke c (depth-color depth) outline rect x y w h))
    (when labels? ;;this can be moved to another layer...
      (doseq [{:keys [x y w h label depth]} coords
              :when label]        
        (push-matrix c)
        (draw-label c  {:s       label
                        :pos     [x y]
                        :shift-y (/ h 2.0)}
                    {:color font-color
                     :font-size (if (zero? depth)
                                  font-size
                                  (* (Math/pow depth-scale depth)
                                     font-size))} 
                    (assoc chart-data :w w :h h))
        (pop-matrix c))))))

;;treemap expects data to come in as a tree.
;;our job is to process it.
(defmethod data-extent :treemap [_ data _]
  (box-extents data))


