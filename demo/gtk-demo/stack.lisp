;;;; Stacks

(in-package #:gtk-demo)

(defun example-stack ()
  (within-main-loop
    (let* ((window (gtk-window-new :toplevel))
           (vbox (make-instance 'gtk-vbox))
           (stack (make-instance 'gtk-stack))
           (switcher (make-instance 'gtk-stack-switcher :stack stack))
           (sidebar (make-instance 'gtk-stack-sidebar :stack stack))
           (button1 (gtk-button-new-with-label "First"))
           (button2 (gtk-button-new-with-label "Second"))
           (button3 (gtk-button-new-with-label "Third")))
      (gtk-stack-add-titled stack button1 "first" "First Title")
      (gtk-stack-add-titled stack button2 "second" "Second Title")
      (gtk-stack-add-titled stack button3 "third" "Third Title")
      (gtk-container-add window vbox)
      (gtk-box-pack-start vbox switcher)
      (gtk-box-pack-start vbox sidebar)
      (gtk-box-pack-start vbox stack)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))
