(in-package :gdk-demo)

(defun put-pixel (pixbuf x y red green blue alpha)
  (let ((n-channels (gdk-pixbuf-n-channels pixbuf))
        (rowstride (gdk-pixbuf-rowstride pixbuf))
        (pixels (gdk-pixbuf-pixels pixbuf)))
    ;; Add offset to the pointer pixels into the pixbuf
    (incf-pointer pixels (+ (* (round y) rowstride) (* (round x) n-channels)))
    ;; Set the color of the point and the alpha value
    (setf (mem-aref pixels :uchar 0) red)
    (setf (mem-aref pixels :uchar 1) green)
    (setf (mem-aref pixels :uchar 2) blue)
    (setf (mem-aref pixels :uchar 3) alpha)))

(defun put-random-pixels (pixbuf x y)
  (let ((width (gdk-pixbuf-width pixbuf))
        (height (gdk-pixbuf-height pixbuf)))

  (dotimes (count 500)

    (let ((xp (+ x (- (random 40) 20)))
          (yp (+ y (- (random 40) 20))))

    (when (< xp 0) (setf xp 0))
    (when (>= xp width) (setf xp width))
    (when (< yp 0) (setf yp 0))
    (when (>= yp height) (setf yp height))

    (put-pixel pixbuf xp yp 255 0 0 255)

  ))))

(defun create-image ()
  (let ((image (gtk-image-new-from-file (rel-path "ducky.png")))
        (event-box (make-instance 'gtk-event-box)))
    ;; Set the event mask for the event box
    (setf (gtk-widget-events event-box) :button-press-mask)
    ;; Connect a signal to the event box
    (g-signal-connect event-box "button-press-event"
                      (lambda (box event)
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

                        (put-random-pixels (gtk-image-pixbuf image)
                                           (gdk-event-button-x event)
                                           (gdk-event-button-y event))

                        (gdk-window-invalidate-rect (gtk-widget-window box) nil t)

                        +gdk-event-stop+))
    ;; Add the image to the event box
    (gtk-container-add event-box image)
    ;; Return the event box with the image
    event-box))

(defun demo-put-pixel ()
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

;;; 2020-11-21
