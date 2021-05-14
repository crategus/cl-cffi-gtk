;;;; Example Upgraded Hello World (2021-5-13)

(in-package :gtk-example)

(defun example-hello-world-upgraded ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (box (gtk-box-new :horizontal 6))
          (button  nil))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (setf (gtk-window-title window) "Hello Buttons")
      (setf (gtk-window-default-size window) '(250 75))
      (setf (gtk-container-border-width window) 12)
      (setf button (gtk-button-new-with-label "Button 1"))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 1 pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (setf button (gtk-button-new-with-label "Button 2"))
      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 2 pressed.~%")))
      (gtk-box-pack-start box button :expand t :fill t :padding 0)
      (gtk-container-add window box)
      (gtk-widget-show-all window))))
