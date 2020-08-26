;;;; Example Event Handler
;;;;
;;;; In this example an event handler ist installed. The event handler
;;;; prints the event on the terminal and passes the event to the
;;;; GTK+ event handler.

(in-package #:gtk-example)

(defun my-event-handler (event)
  (format t "~&in MY-EVENT-HANDLER~%")
  (format t "~a~%" event)
  ;; Pass the event to the GTK+ event handler
  (gtk-main-do-event event))

(defun example-event-handler ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Event Handler"
                                 :type :toplevel
                                 :border-width 12
                                 :default-height 200
                                 :default-width 300)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "~&in Signal Handler DESTROY~%")
                          (format t "~a~%" (gtk-current-event))
                          (format t "~a~%" (gtk-current-event-time))
                          (format t "~a~%" (gtk-current-event-state))
                          (format t "~a~%" (gtk-current-event-device))
                          (leave-gtk-main)))
      ;; Install the new event handler
      (gdk-event-handler-set #'my-event-handler)
      ;; Show the window.
      (gtk-widget-show-all window)))
  (join-gtk-main)
  ;; Install gtk-main-do-event as the event handler
  (gdk-event-handler-set #'gtk-main-do-event))

