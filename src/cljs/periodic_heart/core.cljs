(ns periodic-heart.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(extend-type string ICloneable (-clone [s] (js/String. s)))

(defonce app-state
  (atom {:time 0.0, :time-running false, :interval nil}))

(swap! app-state assoc
       :settings {:primary {:period 1.0
                            :radius 1.0
                            :deg-offset 0
                            :reversing false
                            :color "red"}
                  :pendulum {:period (/ 1 2)
                             :radius (/ 3 4)
                             :deg-offset 0
                             :reversing false
                             :color "green"
                             :from-arc :primary
                             :additive-rotation true
                             :pendulum true}
                  :little-arm {:period (/ 1 8)
                               :radius (/ 1 3)
                               :deg-offset 0
                               :reversing true
                               :color "blue"
                               :from-arc :pendulum
                               :additive-rotation true}}
       :history-length 1500
       :history-offset 0.0004)

(defn fractionize
  "Treat negative values as fractions"
  [n]
  (if (pos? n)
    n
    (/ 1.0 n)))

(defn fractional-part [n] (- n (Math/floor n)))

(defn arc-state [{:keys [period radius
                         deg-offset reversing
                         x-offset y-offset
                         color
                         from-arc
                         additive-rotation
                         pendulum]
                  :or {x-offset 0 y-offset 0 color "black"}
                  :as settings} time arc-source]
  (let [time-offset (* period (/ deg-offset 360))
        parent-arc-config (and arc-source from-arc (arc-source from-arc))
        parent-arc (and parent-arc-config (arc-state parent-arc-config time arc-source))
        parent-rotation (or (and parent-arc additive-rotation
                                 (:rotation parent-arc))
                            0)
        rotations (+  (/ (- time time-offset) period) parent-rotation)
        full-rotations (Math/floor rotations)
        partial-rotations- (fractional-part rotations)
        reversed (and reversing (not (mod full-rotations 2)))
        partial-rotations (if reversed
                            (- 1.0 partial-rotations-)
                            partial-rotations-)
        x-offset (if parent-arc
                   (+ x-offset (:x parent-arc))
                   x-offset)
        y-offset (if parent-arc
                   (+ y-offset (:y parent-arc))
                   y-offset)
        pendulum-radius (* radius (Math/cos (* Math/PI 2 (- rotations parent-rotation))))]
    {:center-x x-offset
     :center-y y-offset
     :x (+ x-offset (* (if pendulum pendulum-radius radius)
                       (Math/cos (* Math/PI 2
                                    (if pendulum parent-rotation partial-rotations)))))
     :y (+ y-offset (* (if pendulum pendulum-radius radius)
                       (Math/sin (* Math/PI 2
                                    (if pendulum parent-rotation partial-rotations)))))
     :interim-x (when pendulum (+ x-offset (* radius (Math/cos (* Math/PI 2 partial-rotations)))))
     :interim-y (when pendulum (+ y-offset (* radius (Math/sin (* Math/PI 2 partial-rotations)))))
     :color color
     :rotation partial-rotations}))

(defn render-canvas [app owner node-ref]
  (let [canvas (om/get-node owner node-ref)
        ctx (.getContext canvas "2d")
        time (:time app)
        circles (:settings app)]
    (.save ctx)
    (.scale ctx 150 150)
    (.clearRect ctx -100 -100 200 200)
    (.translate ctx 3.0 3.0)
    (set! (. ctx -lineWidth) 0.005)
    (doseq [circle-name [:primary :pendulum :little-arm]
            :let [circle (circles circle-name)
                  ]]
      (let [{:keys [x y center-x center-y interim-x interim-y color]
             :or {color "black"}
             :as results} (arc-state circle time circles)]

        (set! (. ctx -globalAlpha) 1.0)
        (set! (. ctx -strokeStyle) color)
        (set! (. ctx -fillStyle) color)

        (.beginPath ctx)
        (.moveTo ctx center-x center-y)
        (when (and interim-x interim-y)
          (.lineTo ctx interim-x interim-y))
        (.lineTo ctx x y)
        (.closePath ctx)
        (.stroke ctx)

        (.beginPath ctx)
        (.arc ctx center-x center-y (:radius circle) 0 (* 2 Math/PI) false)
        (.stroke ctx)
        (.closePath ctx)

        (.beginPath ctx)
        (.arc ctx x y 0.01 0 (* 2 Math/PI) false)
        (.fill ctx)
        (.closePath ctx)))
    (doseq [n (range 1 (:history-length app))
            :let [historical-time (- time (* n (:history-offset app)))
                  {:keys [x y]} (arc-state (:little-arm circles) historical-time circles)]]
      (set! (. ctx -globalAlpha) (* 0.5 (- 1.0 (/ n (:history-length app)))))
      (.beginPath ctx)
      (.arc ctx x y 0.01 0 (* 2 Math/PI) false)
      (.fill ctx)
      (.closePath ctx))
    (.restore ctx)))

(defn handle-change [e cursor owner]
  (om/set-state! owner (.. e -target -value)))

(defn commit-change [text owner]
  (om/set-state! owner :editing false))

(defn numeric-field [label-text]
  (fn [cursor owner]
    (reify
      om/IRender
      (render [_]
        (dom/div
         nil
         (dom/label nil label-text)
         (dom/input #js {:type "text" :value cursor
                         :onChange #(handle-change % cursor owner)}))))))

(defn display [show]
  (if show
    #js {}
    #js {:display "none"}))

(defn editable [text owner]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
      (dom/li
       nil
       (dom/span #js {:style (display (not editing))} (om/value text))
       (dom/input
        #js {:style (display editing)
             :value (om/value text)
             :onChange #(handle-change % text owner)
             :onKeyDown #(when (= (.-key %) "Enter")
                           (commit-change text owner))
             :onBlur (fn [e] (commit-change text owner))})
       (dom/button
        #js {:style (display (not editing))
             :onClick #(om/set-state! owner :editing true)}
        "Edit")))))

(defn main []
  (om/root
   (fn [app owner]
     (reify
       om/IWillMount
       (will-mount [_]
         (om/update! app [:interval]
                     (js/setInterval
                      (fn [_]
                        (om/update! app [:time]
                                    (/ (js/Date.now) 200)))
                      1000)))
       om/IWillUnmount
       (will-unmount [_]
         (js/clearInterval (:interval app)))
       om/IRender
       (render [_]
         (dom/div nil
                  (dom/canvas #js {:ref "canvas" :height 900 :width 1200})
                  (om/build editable (-> app :settings :pendulum :period))
                  ;(om/build (numeric-field "Pendulum Period") (-> app :settings :pendulum :period))
                  ;(om/build (numeric-field "Pendulum Initial Offset") (-> app :settings :pendulum :deg-offset))
                  ))
       om/IDidUpdate
       (did-update [_ _ _]
         (render-canvas app owner "canvas"))
       om/IDidMount
       (did-mount [_]
         (render-canvas app owner "canvas"))))
   app-state
   {:target (. js/document (getElementById "app"))}))
