;;;; Combo Box Text

(in-package #:gtk-demo)

(defun example-combo-box-text ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :border-width 12
                                 :title "Example Combo Box Text"))
          (combo (make-instance 'gtk-combo-box-text)))
      (gtk-combo-box-text-append-text combo "First entry")
      (gtk-combo-box-text-append-text combo "Second entry")
      (gtk-combo-box-text-append-text combo "Third entry")
      (gtk-combo-box-set-active combo 0)
      (gtk-container-add window combo)
      (gtk-widget-show-all window))))

