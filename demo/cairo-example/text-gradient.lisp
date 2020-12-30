(in-package :cairo-example)

(defun drawing-text-gradient (widget cr)
  (let* ((width (gtk-widget-allocated-width widget))
         (height (gtk-widget-allocated-height widget))
         (font-size (truncate (/ height 5)))
         (pattern (cairo-pattern-create-linear 0 15 0 (* 0.8 font-size)))
         (cr (pointer cr)))
    ;; Set the color.
    (cairo-set-source-rgb cr 0.2 0.2 0.2)
    (cairo-paint cr)

    ;; Select the font face
    (cairo-select-font-face cr "Serif" :italic :bold)
    ;; Specify the font size
    (cairo-set-font-size cr font-size)

    (cairo-pattern-set-extend pattern :repeat)
    (cairo-pattern-add-color-stop-rgb pattern 0.0 1.0 0.6 0.0)
    (cairo-pattern-add-color-stop-rgb pattern 0.5 1.0 0.3 0.0)

    (let* ((extents (cairo-text-extents cr "Crategus"))
           (text-width (cairo-text-extents-width extents)))

      ;; Display text on the drawing area
      (cairo-move-to cr (- (/ width 2) (/ text-width 2)) (/ height 2))
      (cairo-text-path cr "Crategus")

      (cairo-set-source cr pattern)
      (cairo-fill cr)
)))

(defun example-text-gradient ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Text Gradient"
                                 :default-width 400
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw" #'drawing-text-gradient)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-28
