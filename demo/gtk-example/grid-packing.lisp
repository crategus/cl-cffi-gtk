(in-package :gtk-example)

(defun create-grid-with-buttons ()
  (let ((grid (make-instance 'gtk-grid :row-spacing 6 :column-spacing 6))
        (button1 (make-instance 'gtk-button :label "Button 1"))
        (button2 (make-instance 'gtk-button :label "Button 2"))
        (button3 (make-instance 'gtk-button :label "Button 3"))
        (button4 (make-instance 'gtk-button :label "Button 4")))

    (gtk-grid-attach grid button1 0 0 2 1)
    (gtk-grid-attach grid button2 1 1 1 2)

    (gtk-grid-attach-next-to grid button3 button1 :right 1 1)
    (gtk-grid-attach-next-to grid button4 button2 :left 1 1)
    grid))

(defun example-grid-packing ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create an add event box with an image to the window
      (gtk-container-add window (create-grid-with-buttons))
      ;; Show the window.
      (gtk-widget-show-all window))))

