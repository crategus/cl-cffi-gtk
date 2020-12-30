(in-package :cairo-example)

(defun example-drawing ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Drawing"
                                 :default-width 400
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw"
          (lambda (widget cr)
            (declare (ignore widget))
              (let ((cr (pointer cr)))
                ;; Draw in black ink.
                (cairo-set-source-rgb cr 0 0 0)
                ;; Choose a font type and set its size.
                (cairo-select-font-face cr "Sans" :normal :normal)
                (cairo-set-font-size cr 20.0)
                ;; Move to a position within the context and draw the text.
                (cairo-move-to cr 10.0 50.0)
                (cairo-show-text cr "Cairo drawing to a widget."))))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-22
