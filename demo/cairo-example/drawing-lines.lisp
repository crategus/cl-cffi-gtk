(in-package :cairo-example)

(defun example-drawing-lines ()
  (within-main-loop
    (let ((path '((10 20) (30 40)))
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Cairo Drawing Lines"
                                 :default-width 400
                                 :default-height 300))
          (area (make-instance 'gtk-drawing-area)))
      (gtk-widget-add-events window :button-press-mask)
      ;; Signal handler for the drawing area
      (g-signal-connect area "draw"
          (lambda (widget cr)
            (declare (ignore widget))
              (let ((cr (pointer cr)))
                ;; Draw in black ink.
                (cairo-set-source-rgb cr 0.0 0.0 1.0)
                (cairo-set-line-width cr 0.5)
                (loop for (x1 y1) in path
                      do (loop for (x2 y2) in path
                               do (cairo-move-to cr x1 y1)
                                  (cairo-line-to cr x2 y2)))
                (cairo-stroke cr)
                (setf path nil))))
      ;; Signal handler for the button press event
      (g-signal-connect window "button-press-event"
                        (lambda (widget event)
                          (case (gdk-event-button event)
                            (1 (push (list (gdk-event-button-x event)
                                           (gdk-event-button-y event))
                                     path))
                            (3 (gtk-widget-queue-draw widget)))))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-container-add window area)
      (gtk-widget-show-all window))))

;;; 2020-12-27
