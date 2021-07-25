;;;; Example Alignment Interactive (2021-7-25)
;;;;
;;;; The gtk-alignment widget controls the alignment and size of its child
;;;; widget. It has four settings: xscale, yscale, xalign, and yalign.
;;;;
;;;; The scale settings are used to specify how much the child widget should
;;;; expand to fill the space allocated to the gtk-alignment. The values can
;;;; range from 0 (meaning the child does not expand at all) to 1 (meaning the
;;;; child expands to fill all of the available space).
;;;;
;;;; The align settings are used to place the child widget within the available
;;;; area. The values range from 0 (top or left) to 1 (bottom or right). Of
;;;; course, if the scale settings are both set to 1, the alignment settings
;;;; have no effect.
;;;;
;;;; This demo allows to change interactively the properties of an alignment.

(in-package :gtk-example)

(defun example-alignment-interactive ()
  (within-main-loop
    (let* (;; Create a toplevel window.
           (window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Alignment Interactive"
                                  :border-width 12))
           ;; A horizontal Box for the content of the window.
           (content (make-instance 'gtk-grid
                                   :orientation :horizontal
                                   :column-spacing 24))
           ;; A vertical Grid for the actions.
           (action (make-instance 'gtk-grid
                                  :orientation :vertical
                                  :row-spacing 6))
           ;; A Frame with a label.
           (frame (make-instance 'gtk-frame
                                 :label "Alignment"
                                 :label-xalign 0.1
                                 :width-request 250
                                 :height-request 250))
           ;; An Alignment
           (alignment (make-instance 'gtk-alignment
                                     :xalign 0
                                     :yalign 0
                                     :xscale 0
                                     :yscale 0))
           ;; A Button which we align and scale
           (button (make-instance 'gtk-button
                                  :label "Button")))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Set the properties x-align and y-align
      (let* ((hbox (make-instance 'gtk-grid
                                  :orientation :horizontal
                                  :column-homogenous t
                                  :column-spacing 6))
             (xalign (gtk-alignment-xalign alignment))
             (x-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value xalign
                                                   :lower 0.0
                                                   :upper 1.0
                                                   :step-increment 0.1
                                                   :page-increment 0.1
                                                   :page-size 0.0)
                                    :climb-rate 0
                                    :digits 2
                                    :wrap t))
             (yalign (gtk-alignment-yalign alignment))
             (y-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value yalign
                                                   :lower 0.0
                                                   :upper 1.0
                                                   :step-increment 0.1
                                                   :page-increment 0.1
                                                   :page-size 0.0)
                                    :climb-rate 0
                                    :digits 2
                                    :wrap t)))
        (g-signal-connect x-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-xalign alignment)
                   (gtk-spin-button-value spin))))
        (g-signal-connect y-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-yalign alignment)
                   (gtk-spin-button-value spin))))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Align Button</b>"))
        (gtk-container-add hbox x-spin)
        (gtk-container-add hbox y-spin)
        (gtk-container-add action hbox))
      ;; Set the properties x-scale and y-scale
      (let* ((hbox (make-instance 'gtk-grid
                                  :orientation :horizontal
                                  :column-homogenous t
                                  :column-spacing 6))
             (xscale (gtk-alignment-xscale alignment))
             (x-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value xscale
                                                   :lower 0.0
                                                   :upper 1.0
                                                   :step-increment 0.1
                                                   :page-increment 0.1
                                                   :page-size 0.0)
                                    :climb-rate 0
                                    :digits 2
                                    :wrap t))
             (yscale (gtk-alignment-yscale alignment))
             (y-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value yscale
                                                   :lower 0.0
                                                   :upper 1.0
                                                   :step-increment 0.1
                                                   :page-increment 0.1
                                                   :page-size 0.0)
                                    :climb-rate 0
                                    :digits 2
                                    :wrap t)))
        (g-signal-connect x-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-xscale alignment)
                   (gtk-spin-button-value spin))))
        (g-signal-connect y-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-yscale alignment)
                   (gtk-spin-button-value spin))))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Scale Button</b>"))
        (gtk-container-add hbox x-spin)
        (gtk-container-add hbox y-spin)
        (gtk-container-add action hbox))
      ;; Set the properties top-padding and bottom-padding
      (let* ((hbox (make-instance 'gtk-grid
                                  :orientation :horizontal
                                  :column-homogenous t
                                  :column-spacing 6))
             (top-padding (gtk-alignment-top-padding alignment))
             (x-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value top-padding
                                                   :lower 0
                                                   :upper 200
                                                   :step-increment 2
                                                   :page-increment 1
                                                   :page-size 0)
                                    :climb-rate 0
                                    :digits 0
                                    :wrap t))
             (bottom-padding (gtk-alignment-bottom-padding alignment))
             (y-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value bottom-padding
                                                   :lower 0
                                                   :upper 200
                                                   :step-increment 2
                                                   :page-increment 1
                                                   :page-size 0)
                                    :climb-rate 0
                                    :digits 0
                                    :wrap t)))
        (g-signal-connect x-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-top-padding alignment)
                   (truncate (gtk-spin-button-value spin)))))
        (g-signal-connect y-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-bottom-padding alignment)
                   (truncate (gtk-spin-button-value spin)))))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Top/Bottom Padding</b>"))
        (gtk-container-add hbox x-spin)
        (gtk-container-add hbox y-spin)
        (gtk-container-add action hbox))
      ;; Set the properties left-padding and right-padding
      (let* ((hbox (make-instance 'gtk-grid
                                  :orientation :horizontal
                                  :column-homogenous t
                                  :column-spacing 6))
             (left-padding (gtk-alignment-left-padding alignment))
             (x-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value left-padding
                                                   :lower 0
                                                   :upper 200
                                                   :step-increment 2
                                                   :page-increment 1
                                                   :page-size 0)
                                    :climb-rate 0
                                    :digits 0
                                    :wrap t))
             (right-padding (gtk-alignment-right-padding alignment))
             (y-spin (make-instance 'gtk-spin-button
                                    :adjustment
                                    (make-instance 'gtk-adjustment
                                                   :value right-padding
                                                   :lower 0
                                                   :upper 200
                                                   :step-increment 2
                                                   :page-increment 1
                                                   :page-size 0)
                                    :climb-rate 0
                                    :digits 0
                                    :wrap t)))
        (g-signal-connect x-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-left-padding alignment)
                   (truncate (gtk-spin-button-value spin)))))
        (g-signal-connect y-spin "value-changed"
           (lambda (spin)
             (setf (gtk-alignment-right-padding alignment)
                   (truncate (gtk-spin-button-value spin)))))
        (gtk-container-add action
                           (make-instance 'gtk-label
                                          :use-markup t
                                          :xalign 0.0
                                          :margin-top 12
                                          :label
                                          "<b>Left/Right Padding</b>"))
        (gtk-container-add hbox x-spin)
        (gtk-container-add hbox y-spin)
        (gtk-container-add action hbox))
      ;; Add button, alignment, frame, content, and action to the window.
      (gtk-container-add alignment button)
      (gtk-container-add frame alignment)
      (gtk-container-add content frame)
      (gtk-container-add content action)
      (gtk-container-add window content)
      ;; Show the window.
      (gtk-widget-show-all window))))
