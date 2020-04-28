(in-package :gtk-example)

(defun create-event-box ()
  (let ((image (gtk-image-new-from-file (rel-path "ducky.png")))
        (event-box (make-instance 'gtk-event-box)))
    ;; Set the event mask for the event box
    (setf (gtk-widget-events event-box) :button-press-mask)
    ;; Connect a signal to the event box
    (g-signal-connect event-box "button-press-event"
                      (lambda (box event)
                        ;; x,y for pointer from event
                        (format t "~%Event box clicked at   : ~6,2f, ~6,2f~%"
                                  (gdk-event-button-x event)
                                  (gdk-event-button-y event))
                        ;; x,y for pointer from widget (deprectated)
                        (multiple-value-bind (x y)
                            (gtk-widget-pointer box)
                          (format t "Pointer position       : ~d, ~d~%" x y))
                        (let* ((seat (gdk-display-default-seat (gdk-display-default)))
                               (device (gdk-seat-pointer seat)))
                          ;; x,y, for pointer from device position
                          (multiple-value-bind (win x y mask)
                              (gdk-window-device-position (gtk-widget-window event-box) device)
                            (declare (ignore win mask))
                            (format t "Device Position        : ~d, ~d~%" x y))
                          ;; x,y for pointer from device position in double precision
                          (multiple-value-bind (win x y mask)
                              (gdk-window-device-position-double (gtk-widget-window event-box) device)
                            (declare (ignore win mask))
                            (format t "Device Position double : ~6,2f, ~6,2f~%" x y)))
                        +gdk-event-stop+))
    ;; Add the image to the event box
    (gtk-container-add event-box image)
    ;; Return the event box with the image
    event-box))

(defun example-widget-pointer ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create an add event box with an image to the window
      (gtk-container-add window (create-event-box))
      ;; Show the window.
      (gtk-widget-show-all window))))

