;;;; Drawing in response to input

(let ((surface nil))
  (defun example-drawing ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :type :toplevel
                                   :title "Example Drawing"
                                   :border-width 12))
            (frame (make-instance 'gtk-frame
                                  :shadow-type :in))
            (area (make-instance 'gtk-drawing-area
                                 :width-request 250
                                 :height-request 200)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signals used to handle the backing surface
        (g-signal-connect area "draw"
           (lambda (widget cr)
             (declare (ignore widget))
             (cairo-set-source-surface (pointer cr) surface 0.0d0 0.0d0)
             (cairo-paint (pointer cr))
             nil))
        (g-signal-connect area "configure-event"
           (lambda (widget event)
             (declare (ignore event))
             (when surface
               (cairo-surface-destroy surface))
             (setq surface
                   (gdk-window-create-similar-surface
                                   (gtk-widget-get-window widget)
                                   :color
                                   (gtk-widget-get-allocated-width widget)
                                   (gtk-widget-get-allocated-height widget)))
             ;; Clear surface
             (let ((cr (cairo-create surface)))
               (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
               (cairo-paint cr)
               (cairo-destroy cr))
             (format t "leave event 'configure-event'~%")
             t))
        ;; Event signals
        (g-signal-connect area "motion-notify-event"
           (lambda (widget event)
             (format t "MOTION-NOTIFY-EVENT ~A~%" event)
             (when (member :button1-mask (gdk-event-motion-state event))
               (let ((cr (cairo-create surface))
                     (x (gdk-event-motion-x event))
                     (y (gdk-event-motion-y event)))
                 (cairo-rectangle cr (- x 3.0d0) (- y 3.0d0) 6.0d0 6.0d0)
                 (cairo-fill cr)
                 (cairo-destroy cr)
                 (gtk-widget-queue-draw-area widget
                                             (truncate (- x 3.0d0))
                                             (truncate (- y 3.0d0))
                                             6
                                             6)))
             ;; We have handled the event, stop processing
             t))
        (g-signal-connect area "button-press-event"
           (lambda (widget event)
             (format t "BUTTON-PRESS-EVENT ~A~%" event)
             (if (eql 1 (gdk-event-button-button event))
                 (let ((cr (cairo-create surface))
                       (x (gdk-event-button-x event))
                       (y (gdk-event-button-y event)))
                   (cairo-rectangle cr (- x 3.0d0) (- y 3.0d0) 6.0d0 6.0d0)
                   (cairo-fill cr)
                   (cairo-destroy cr)
                   (gtk-widget-queue-draw-area widget
                                               (truncate (- x 3.0d0))
                                               (truncate (- y 3.0d0))
                                               6
                                               6))
                 ;; Clear surface
                 (let ((cr (cairo-create surface)))
                   (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
                   (cairo-paint cr)
                   (cairo-destroy cr)
                   (gtk-widget-queue-draw widget)))))
        (gtk-widget-set-events area
                               (append (gtk-widget-get-events area)
                                       '(:button-press-mask
                                         :pointer-motion-mask)))
        (gtk-container-add frame area)
        (gtk-container-add window frame)
        (gtk-widget-show-all window)))))

