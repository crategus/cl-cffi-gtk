;;;; Paned Window Widgets (2021-5-28)

(in-package :gtk-example)

(defun example-paned-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Paned Window"
                                 :default-width 320
                                 :default-height 280
                                 :border-width 12))
          (paned (make-instance 'gtk-paned
                                :position 100
                                :orientation :vertical))
          (frame1 (make-instance 'gtk-frame
                                 :label "Window 1"
                                 :label-yalign 0.0))
          (frame2 (make-instance 'gtk-frame
                                 :label "Window 2"
                                 :label-yalign 0.0)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window paned)
      (gtk-paned-add1 paned frame1)
      (gtk-paned-add2 paned frame2)
      (gtk-widget-show-all window))))
