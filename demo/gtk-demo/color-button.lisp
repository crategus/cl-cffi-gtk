;;;; Color Button

(in-package #:gtk-demo)

(let ((color (gdk-rgba-parse "Gray")))
  (defun example-color-button ()
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Example Color Button"
                                   :border-width 12
                                   :default-width 250
                                   :default-height 200))
            (button (make-instance 'gtk-color-button
                                   :rgba color)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g-signal-connect button "color-set"
           (lambda (widget)
             (let ((rgba (gtk-color-chooser-rgba widget)))
               (format t "Selected color is ~A~%"
                       (gdk-rgba-to-string rgba)))))
        (gtk-container-add window button)
        (gtk-widget-show-all window)))))

