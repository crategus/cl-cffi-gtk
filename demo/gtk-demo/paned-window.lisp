;;;; Paned Window Widgets

(in-package #:gtk-demo)

(defun example-paned-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Paned Window"
                                 :border-width 12))
          (paned (make-instance 'gtk-paned
                                :orientation :vertical))
          (frame1 (make-instance 'gtk-frame :label "Window 1"))
          (frame2 (make-instance 'gtk-frame :label "Window 2")))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (setf (gtk-widget-size-request window) '(300 250))
      (gtk-container-add window paned)
      (gtk-paned-add1 paned frame1)
      (gtk-paned-add2 paned frame2)
      (gtk-widget-show-all window))))
