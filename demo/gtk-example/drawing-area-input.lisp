;;;; Example Drawing in response to input (2021-5-13)

(in-package :gtk-example)

(defun example-drawing-area-input ()
  (within-main-loop
    (let ((surface nil)
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Drawing"))
          (area (make-instance 'gtk-drawing-area
                               :width-request 320
                               :height-request 240)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Signals used to handle the backing surface
      (g-signal-connect area "draw"
         (lambda (widget cr)
           (declare (ignore widget))
           (let ((cr (pointer cr)))
             (cairo-set-source-surface cr surface 0.0 0.0)
             (cairo-paint cr)
             (cairo-destroy cr)
             +gdk-event-propagate+)))
      (g-signal-connect area "configure-event"
         (lambda (widget event)
           (declare (ignore event))
           (when surface
             (cairo-surface-destroy surface))
           (setf surface
                 (gdk-window-create-similar-surface
                                 (gtk-widget-window widget)
                                 :color
                                 (gtk-widget-allocated-width widget)
                                 (gtk-widget-allocated-height widget)))
           ;; Clear surface
           (let ((cr (cairo-create surface)))
             (cairo-set-source-rgb cr 1.0 1.0 1.0)
             (cairo-paint cr)
             (cairo-destroy cr))
           (format t "leave event 'configure-event'~%")
           +gdk-event-stop+))
      ;; Event signals
      (g-signal-connect area "motion-notify-event"
         (lambda (widget event)
           (format t "MOTION-NOTIFY-EVENT ~A~%" event)
           (when (member :button1-mask (gdk-event-motion-state event))
             (let ((cr (cairo-create surface))
                   (x (gdk-event-motion-x event))
                   (y (gdk-event-motion-y event)))
               (cairo-rectangle cr (- x 3.0) (- y 3.0) 6.0 6.0)
               (cairo-fill cr)
               (cairo-destroy cr)
               (gtk-widget-queue-draw-area widget
                                           (truncate (- x 3.0))
                                           (truncate (- y 3.0))
                                           6
                                           6)))
           ;; We have handled the event, stop processing
           +gdk-event-stop+))
      (g-signal-connect area "button-press-event"
         (lambda (widget event)
           (format t "BUTTON-PRESS-EVENT ~A~%" event)
           (if (= 1 (gdk-event-button-button event))
               (let ((cr (cairo-create surface))
                     (x (gdk-event-button-x event))
                     (y (gdk-event-button-y event)))
                 (cairo-rectangle cr (- x 3.0) (- y 3.0) 6.0 6.0)
                 (cairo-fill cr)
                 (cairo-destroy cr)
                 (gtk-widget-queue-draw-area widget
                                             (truncate (- x 3.0))
                                             (truncate (- y 3.0))
                                             6
                                             6))
               ;; Clear surface
               (let ((cr (cairo-create surface)))
                 (cairo-set-source-rgb cr 1.0 1.0 1.0)
                 (cairo-paint cr)
                 (cairo-destroy cr)
                 (gtk-widget-queue-draw widget)))))
      (gtk-widget-add-events area
                             '(:button-press-mask
                               :pointer-motion-mask))
      (gtk-container-add window area)
      (gtk-widget-show-all window))))
