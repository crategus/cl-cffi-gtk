(in-package :pango-example)

(defun drawing-text-centered (widget cr)
  (let* ((width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (cr (pointer cr))
         (layout (pango-cairo-create-layout cr)))
    ;; Set the color
    (cairo-set-source-rgb cr 0.5 0.5 0.5)
    ;; Set the font
    (setf (pango-layout-font-description layout)
          (pango-font-description-from-string "Courier Bold 60"))
    ;; Set the text
    (setf (pango-layout-text layout) "Crategus")
    ;; Extents of the layout in pixel Units
    (multiple-value-bind (text-width text-height)
        (pango-layout-pixel-size layout)
      ;; Move to the start position of the text
      (cairo-move-to cr (- (/ width 2) (/ text-width 2))
                        (- (/ height 2) (/ text-height 2)))
      ;; Print the text on the Cario context
      (pango-cairo-show-layout cr layout)
      (cairo-destroy cr))))

(defun example-text-centered ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Pango Text Centered"
                                 :default-width 400
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-text-centered)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2021-1-16
