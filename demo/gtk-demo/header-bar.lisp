(in-package #:gtk-demo)

(defun example-header-bar ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (bar (make-instance 'gtk-header-bar :title "Title" :subtitle "Subtitle" :show-close-button T))
          (label1 (gtk-label-new "Start Label"))
          (label2 (gtk-label-new "End Label")))
      (gtk-header-bar-pack-start bar label1)
      (gtk-header-bar-pack-end bar label2)
      (gtk-container-add window bar)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))

(defun example-title-bar ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (bar (make-instance 'gtk-header-bar :title "Title" :subtitle "Subtitle" :show-close-button T))
          (label1 (gtk-label-new "Start Label"))
          (label2 (gtk-label-new "End Label")))
      (gtk-header-bar-pack-start bar label1)
      (gtk-header-bar-pack-end bar label2)
      (gtk-window-set-titlebar window bar)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))
