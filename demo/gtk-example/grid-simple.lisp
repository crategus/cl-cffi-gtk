;;;; Example Simple Grid (2021-5-15)

(in-package :gtk-example)

(defun example-grid-simple ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Simple Grid"
                                 :border-width 12
                                 :default-width 320))
          (grid (make-instance 'gtk-grid
                               :column-homogeneous t
                               :column-spacing 6
                               :row-homogeneous t
                               :row-spacing 6))
          (button1 (make-instance 'gtk-button
                                  :label "Button 1"))
          (button2 (make-instance 'gtk-button
                                  :label "Button 2"))
          (button3 (make-instance 'gtk-button
                                  :label "Button 3")))

      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-grid-attach grid button1 0 0 1 1)
      (gtk-grid-attach grid button2 1 0 1 1)
      (gtk-grid-attach grid button3 0 1 2 1)

      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
