(in-package :gtk-example)

(defun create-event-box-for-pointer-device ()
  (let ((image (gtk-image-new-from-file (sys-path "ducky.png")))
        (event-box (make-instance 'gtk-event-box)))
    ;; Set the event mask for the event box
    (setf (gtk-widget-events event-box) :button-press-mask)
    ;; Connect a signal to the event box
    (g-signal-connect event-box "button-press-event"
        (lambda (box event)
          (declare (ignore box event))
          (let* ((seat (gdk-display-default-seat (gdk-display-default)))
                 (device (gdk-seat-pointer seat)))
            ;; x,y, for pointer from device position
            (multiple-value-bind (window x y mask)
                (gdk-window-device-position (gtk-widget-window event-box) device)
              (declare (ignore window mask))
              (format t "~%")
              (format t "type         : ~a~%" (gdk-device-type device))
              (format t "associated   : ~a~%" (gdk-device-associated-device device))
              (format t "axes         : ~a~%" (gdk-device-axes device))
              (format t "manager      : ~a~%" (gdk-device-device-manager device))
              (format t "display      : ~a~%" (gdk-device-display device))
              (format t "has-cursor   : ~a~%" (gdk-device-has-cursor device))
              (format t "input-mode   : ~a~%" (gdk-device-input-mode device))
              (format t "input-source : ~a~%" (gdk-device-input-source device))
              (format t "n-axes       : ~a~%" (gdk-device-n-axes device))
              (format t "name         : ~a~%" (gdk-device-name device))
              (format t "num-touches  : ~a~%" (gdk-device-num-touches device))
              (format t "product-id   : ~a~%" (gdk-device-product-id device))
              (format t "seat         : ~a~%" (gdk-device-seat device))
              (format t "tool         : ~a~%" (gdk-device-tool device))
              (format t "vendor-id    : ~a~%" (gdk-device-vendor-id device))


              (format t "Position     : ~d, ~d~%" x y)
              (format t "History      : ~a~%~%"
                        (gdk-device-history device (gtk-widget-window event-box) 0 100))
          ))
                +gdk-event-stop+))
    ;; Add the image to the event box
    (gtk-container-add event-box image)
    ;; Return the event box with the image
    event-box))

(defun example-pointer-device ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create an add event box with an image to the window
      (gtk-container-add window (create-event-box-for-pointer-device))
      ;; Show the window.
      (gtk-widget-show-all window))))

;; 2020-11-5
