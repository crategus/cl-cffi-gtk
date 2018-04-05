(in-package #:gtk-demo)

(defun example-action-bar ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (bar (make-instance 'gtk-action-bar))
          (button1 (gtk-button-new-with-label "Start"))
          (button2 (gtk-button-new-with-label "Middle"))
          (button3 (gtk-button-new-with-label "End")))
      (gtk-action-bar-pack-start bar button1)
      (setf (gtk-action-bar-center-widget bar) button2)
      (gtk-action-bar-pack-end bar button3)
      (gtk-container-add window bar)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))
