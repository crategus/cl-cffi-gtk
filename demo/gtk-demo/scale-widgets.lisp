;;;; Scale Widgets

(in-package #:gtk-demo)

(defun example-scale-widgets ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Scale Widgets"))
           (box1 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 0))
           (box2 (make-instance 'gtk-box
                                :orientation :horizontal
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
           (box3 (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12))
           (adj1 (make-instance 'gtk-adjustment
                                :value 0.0
                                :lower 0.0
                                :upper 101.0
                                :step-increment 0.1
                                :page-increment 1.0
                                :page-size 1.0))
           (vscale (make-instance 'gtk-scale
                                  :orientation :vertical
                                  :digits 1
                                  :value-pos :top
                                  :draw-value t
                                  :adjustment adj1))
           (hscale (make-instance 'gtk-scale
                                   :orientation :horizontal
                                   :digits 1
                                   :value-pos :top
                                   :draw-value t
                                   :width-request 200
                                   :height-request -1
                                   :adjustment adj1))
           (scrollbar (make-instance 'gtk-scrollbar
                                     :orientation :horizontal
                                     :adjustment adj1)))
      ;; Connect a handler for the signal "destroy" to the main window.
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Packing of the global widgets hscale, vscale, and scrollbar
      (gtk-container-add window box1)
      (gtk-box-pack-start box1 box2)
      (gtk-box-pack-start box2 vscale)
      (gtk-box-pack-start box2 box3)
      (gtk-box-pack-start box3 hscale)
      (gtk-box-pack-start box3 scrollbar)
      ;; A check button to control whether the value is displayed or not.
      (let ((box (make-instance 'gtk-box
                                :orientation :horizontal
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (button (make-instance 'gtk-check-button
                                   :label "Display value on scale widget"
                                   :active t)))
        (g-signal-connect button "toggled"
           (lambda (widget)
             (gtk-scale-set-draw-value
                                     hscale
                                     (gtk-toggle-button-active widget))
             (gtk-scale-set-draw-value
                                     vscale
                                     (gtk-toggle-button-active widget))))
        (gtk-box-pack-start box button)
        (gtk-box-pack-start box1 box))
      ;; A ComboBox to change the position of the value.
      (let ((box (make-instance 'gtk-box
                                :orientation :horizontal
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (combo (make-instance 'gtk-combo-box-text)))
        (gtk-combo-box-text-append-text combo "TOP")
        (gtk-combo-box-text-append-text combo "BOTTOM")
        (gtk-combo-box-text-append-text combo "LEFT")
        (gtk-combo-box-text-append-text combo "RIGHT")
        (gtk-combo-box-set-active combo 0)
        (g-signal-connect combo "changed"
           (lambda (widget)
             (let ((pos (gtk-combo-box-text-get-active-text widget)))
               (format t "type      : ~A~%"
                         (g-type-from-instance (pointer widget)))
               (format t "active is : ~A~%"
                         (gtk-combo-box-get-active widget))
               (setq pos (if pos (intern pos :keyword) :top))
               (gtk-scale-set-value-pos hscale pos)
               (gtk-scale-set-value-pos vscale pos))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scale value position")
                            :expand nil :fill nil :padding 0)
        (gtk-box-pack-start box combo)
        (gtk-box-pack-start box1 box))
      ;; Create a scale to change the digits of hscale and vscale.
      (let* ((box (make-instance 'gtk-box
                                 :orientation :horizontal
                                 :homogeneous nil
                                 :spacing 12
                                 :border-width 12))
             (adj (make-instance 'gtk-adjustment
                                 :value 1.0
                                 :lower 0.0
                                 :upper 5.0
                                 :step-increment 1.0
                                 :page-increment 1.0
                                 :page-size 0.0))
             (scale (make-instance 'gtk-scale
                                   :orientation :horizontal
                                   :digits 0
                                   :adjustment adj)))
        (g-signal-connect adj "value-changed"
           (lambda (adjustment)
             (setf (gtk-scale-digits hscale)
                   (truncate (gtk-adjustment-value adjustment)))
             (setf (gtk-scale-digits vscale)
                   (truncate (gtk-adjustment-value adjustment)))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scale Digits:")
                            :expand nil :fill nil)
        (gtk-box-pack-start box scale)
        (gtk-box-pack-start box1 box))
      ;; Another hscale for adjusting the page size of the scrollbar
      (let* ((box (make-instance 'gtk-box
                                 :orientation :horizontal
                                 :homogeneous nil
                                 :spacing 12
                                 :border-width 12))
             (adj (make-instance 'gtk-adjustment
                                 :value 1.0
                                 :lower 1.0
                                 :upper 101.0
                                 :step-increment 1.0
                                 :page-increment 1.0
                                 :page-size 0.0))
             (scale (make-instance 'gtk-scale
                                   :orientation :horizontal
                                   :digits 0
                                   :adjustment adj)))
        (g-signal-connect adj "value-changed"
           (lambda (adjustment)
             (setf (gtk-adjustment-page-size adj1)
                   (gtk-adjustment-page-size adjustment))
             (setf (gtk-adjustment-page-increment adj1)
                   (gtk-adjustment-page-increment adjustment))))
        (gtk-box-pack-start box
                            (make-instance 'gtk-label
                                           :label "Scrollbar Page Size:")
                            :expand nil :fill nil)
        (gtk-box-pack-start box scale)
        (gtk-box-pack-start box1 box))
      ;; Add a separator
      (gtk-box-pack-start box1
                          (make-instance 'gtk-separator
                                         :orientation :horizontal)
                          :expand nil :fill t)
      ;; Create the quit button.
      (let ((box (make-instance 'gtk-box
                                :orientation :vertical
                                :homogeneous nil
                                :spacing 12
                                :border-width 12))
            (button (make-instance 'gtk-button :label "Quit")))
        (g-signal-connect button "clicked"
                          (lambda (button)
                            (declare (ignore button))
                            (gtk-widget-destroy window)))
        (gtk-box-pack-start box button)
        (gtk-box-pack-start box1 box :expand nil))
      (gtk-widget-show-all window))))

