;;;; Color Button
;;;;
;;;; The example shows a color button. The button is initialized with the color
;;;; "Blue". The handler for the "color-set" signal prints the selected color
;;;; on the console.

(in-package #:gtk-demo)

(defun example-color-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Color Button"
                                 :border-width 12
                                 :default-width 250
                                 :default-height 200))
          (button (make-instance 'gtk-color-button
                                 :rgba (gdk-rgba-parse "Blue"))))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect button "color-set"
         (lambda (widget)
           (let ((rgba (gtk-color-chooser-rgba widget)))
             (format t "Selected color is ~A~%" (gdk-rgba-to-string rgba)))))
      (gtk-container-add window button)
      (gtk-widget-show-all window))))

;;; 2021-2-3
