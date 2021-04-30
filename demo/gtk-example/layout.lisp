;;;; Example Layout widget (2021-4-26)

(in-package :gtk-example)

(defun example-layout ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Layout Widget"
                                 :width-request 360
                                 :height-request 240))
          (layout (make-instance 'gtk-layout)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-layout-put layout
                      (make-instance 'gtk-button
                                     :label "Button 1")
                      40 60)
      (gtk-layout-put layout
                      (make-instance 'gtk-button
                                     :label "Button 2")
                      120 105)
      ;; Pack and show the widgets
      (gtk-container-add window layout)
      (gtk-widget-show-all window))))
