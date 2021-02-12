;;;; Color Chooser Widget

(in-package #:gtk-demo)

(defun example-color-chooser-widget ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Color Chooser Widget"
                                 :border-width 12
                                 :default-width 400))
          (color-chooser (make-instance 'gtk-color-chooser-widget)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g-signal-connect color-chooser "color-activated"
            (lambda (chooser color)
              (declare (ignore chooser))
              (format t "Selected color is ~a~%" (gdk-rgba-to-string color))))
        (gtk-container-add window color-chooser)
        (gtk-widget-show-all window))))

;;; 2021-2-4
