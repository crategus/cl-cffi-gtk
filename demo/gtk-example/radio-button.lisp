;;;; Example Radio Button (2021-5-21)

(in-package :gtk-example)

(defun example-radio-button ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Radio Button"
                                 :border-width 12
                                 :default-width 300
                                 :default-height 120))
          (grid (make-instance 'gtk-grid
                               :orientation :vertical
                               :halign :center
                               :valign :center
                               :row-spacing 18)))

      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (let ((radio (gtk-radio-button-new nil)))
        (gtk-container-add radio (gtk-entry-new))
        (gtk-container-add grid radio)
        (setf radio
              (gtk-radio-button-new-with-label-from-widget radio
                                                           "Second Button"))
        (gtk-container-add grid radio))

      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
