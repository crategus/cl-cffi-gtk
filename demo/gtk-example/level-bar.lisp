;;;; Example Level Bar - 2021-4-16

(in-package :gtk-example)

(defun create-level-bar (orientation mode)
  (let ((provider (gtk-css-provider-new))
        (levelbar (make-instance 'gtk-level-bar
                                  :orientation orientation
                                  :mode mode)))
    ;; Change values from 0 to 10
    (setf (gtk-level-bar-max-value levelbar) 10)
    ;; Add new offset value with name "empty"
    (gtk-level-bar-add-offset-value levelbar "empty" 2.0)
    ;; Adjust the standard offset values
    (gtk-level-bar-add-offset-value levelbar "low" 4.0)
    (gtk-level-bar-add-offset-value levelbar "high" 8.0)
    (gtk-level-bar-add-offset-value levelbar "full" 10.0)
    ;; CSS to change the color for the values
    (gtk-css-provider-load-from-data provider
                                     "levelbar block.filled.empty {
                                        background-color: red; }
                                      levelbar block.filled.low {
                                        background-color: orange; }
                                      levelbar block.filled.high {
                                        background-color: yellow; }
                                      levelbar block.filled.full {
                                        background-color: green; }")
    (gtk-style-context-add-provider (gtk-widget-style-context levelbar)
                                    provider
                                    +gtk-style-provider-priority-application+)
    ;; Return the new level bar
    levelbar))

(defun example-level-bar ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Level bar"
                                  :border-width 12
                                  :default-width 420
                                  :default-height 240))
           (vbox (make-instance 'gtk-box
                                :orientation :vertical
                                :spacing 12))
           (adj (make-instance 'gtk-adjustment
                               :value 0.0
                               :lower 0.0
                               :upper 10.0
                               :step-increment 0.1))
           (scale (make-instance 'gtk-scale
                                 :orientation :horizontal
                                 :digits 1
                                 :value-pos :top
                                 :draw-value t
                                 :adjustment adj))
           (levelbar1 (create-level-bar :horizontal :continuous))
           (levelbar2 (create-level-bar :horizontal :discrete)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Bind adjustment value for the scale to the level bar values
      (g-object-bind-property adj "value" levelbar1 "value" :default)
      (g-object-bind-property adj "value" levelbar2 "value" :default)
      ;; Pack and show the widgets
      (gtk-box-pack-start vbox (make-instance 'gtk-label
                                              :xalign 0.0
                                              :use-markup t
                                              :label "<b>Continuous mode</b>")
                               :expand nil)
      (gtk-box-pack-start vbox levelbar1)
      (gtk-box-pack-start vbox (make-instance 'gtk-label
                                              :xalign 0.0
                                              :use-markup t
                                              :label "<b>Discrete mode</b>")
                               :expand nil)
      (gtk-box-pack-start vbox levelbar2)
      (gtk-box-pack-start vbox (make-instance 'gtk-label
                                              :use-markup t
                                              :xalign 0.0
                                              :label "<b>Change value</b>")
                               :expand nil)
      (gtk-box-pack-start vbox scale :expand nil)
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
