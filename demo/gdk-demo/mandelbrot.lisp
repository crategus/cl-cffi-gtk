(in-package :gdk-demo)

(defun put-pixel (pixbuf x y red green blue alpha)
  (let ((n-channels (gdk-pixbuf-n-channels pixbuf))
        (rowstride (gdk-pixbuf-rowstride pixbuf))
        (pixels (gdk-pixbuf-read-pixels pixbuf)))
    ;; Add offset to the pointer pixels into the pixbuf
    (incf-pointer pixels (+ (* y rowstride) (* x n-channels)))
    ;; Set the color of the point and the alpha value
    (setf (mem-aref pixels :uchar 0) red)
    (setf (mem-aref pixels :uchar 1) green)
    (setf (mem-aref pixels :uchar 2) blue)
    (setf (mem-aref pixels :uchar 3) alpha)))

(defun iterate-mandelbrot (image iterate-max)
  (let* ((color 0)
         (iterate 0)
         (x 0.0d0) (x0 0.0d0)
         (y 0.0d0) (y0 0.0d0)
         (pixbuf (gtk-image-pixbuf image))
         (width (gdk-pixbuf-width pixbuf))
         (height (gdk-pixbuf-height pixbuf)))

    (dotimes (i (+ width 1))
      (dotimes (j (+ height 1))

        (setf x0 (coerce (+ -2 (/ (* 3 i) width)) 'double-float))
        (setf y0 (coerce (+ -1 (/ (* 2 j) height)) 'double-float))

        (setf iterate 0)

        (setf x 0.0d0)
        (setf y 0.0d0)

        (do ((xtemp 0.0d0)
             (iterate 0 (+ iterate 1)))
            ((or (>= iterate iterate-max) (> (+ (* x x) (* y y)) 4))
             (setf color (+ 10 (* 2 iterate))))
          (setf xtemp (+ x0 (- (* x x) (* y y))))
          (setf y (+ y0 (* 2 x y)))
          (setf x xtemp))

        (if (>= color iterate-max)
            (put-pixel pixbuf i j 0 0 0 255)

            (put-pixel pixbuf i j (logand color #xff)
                                  (/ (logand color #xff00) 256)
                                  (/ (logand color #xff0000) (* 256 256))
                                  255))
      ))))

(defun create-image ()
  (let ((image (make-instance 'gtk-image
                              :pixbuf (gdk-pixbuf-new :rgb t 8 1200 800)))
        (event-box (make-instance 'gtk-event-box)))
    ;; Set the event mask for the event box
    (setf (gtk-widget-events event-box) :button-press-mask)
    ;; Connect a signal to the event box
    (g-signal-connect event-box "button-press-event"
                      (lambda (box event)
                        (declare (ignore box event))

                        (format t "Start mandelbrot iteration~&")

                        (iterate-mandelbrot image 20000)
                        (gtk-widget-show image)
                        (format t "Finished mandelbrot iteration~%")

                        +gdk-event-stop+))
    ;; Add the image to the event box
    (gtk-container-add event-box image)
    ;; Return the event box with the image
    event-box))

(defun demo-mandelbrot ()
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

;;; 2020-11-23
