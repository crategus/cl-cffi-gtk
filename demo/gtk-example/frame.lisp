;;;; Example Frame - 2021-11-5

(in-package :gtk-example)

(defun example-frame (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :application application
                                 :title "Example Frame"
                                 :default-width 250
                                 :default-height 200
                                 :border-width 12))
          (frame (make-instance 'gtk-frame
                                :label "Frame Label"
                                :label-xalign 1.0
                                :label-yalign 0.5
                                :shadow-type :etched-in)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-container-add window frame)
      (gtk-widget-show-all window))))
