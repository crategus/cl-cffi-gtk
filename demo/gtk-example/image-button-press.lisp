(in-package :gtk-example)

(defun create-image ()
  (let ((image (gtk-image-new-from-file (rel-path "ducky.png")))
        (event-box (make-instance 'gtk-event-box)))
    ;; Set the event mask for the event box
    (setf (gtk-widget-events event-box) :button-press-mask)
    ;; Connect a signal to the event box
    (g-signal-connect event-box "button-press-event"
                      (lambda (box event)
                        (declare (ignore box))
                        ;; Print infos about the event
                        (format t "~&in Signal Handler BUTTON-PRESS-EVENT~%")
                        (format t "~a~%" (gtk-current-event))
                        (format t "~a~%" (gtk-current-event-time))
                        (format t "~a~%" (gtk-current-event-state))
                        (format t "~a~%" (gtk-current-event-device))
                        ;; Print position of mouse click
                        (format t "Event box clicked at : ~6,2f, ~6,2f~%"
                                  (gdk-event-button-x event)
                                  (gdk-event-button-y event))
                        +gdk-event-stop+))
    ;; Add the image to the event box
    (gtk-container-add event-box image)
    ;; Return the event box with the image
    event-box))

(defun example-image-button-press ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create an add event box with an image to the window
      (gtk-container-add window (create-image))
      ;; Show the window.
      (gtk-widget-show-all window))))

