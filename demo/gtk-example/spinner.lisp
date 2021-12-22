;;;; Example Spinner - 2021-12-21
;;;;
;;;; GtkSpinner allows to show that background activity is on-going.

(in-package :gtk-example)

(defun example-spinner (&optional application)
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Spinner"
                                 :application application))
          (vbox (make-instance 'gtk-box
                               :orientation :vertical
                               :margin 6
                               :spacing 3))
          (sensitive (gtk-spinner-new))
          (insensitive (gtk-spinner-new)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (let ((hbox (make-instance 'gtk-box
                                 :orientation :horizontal
                                 :sensitive t)))
        (gtk-box-pack-start hbox sensitive)
        (gtk-box-pack-start hbox (make-instance 'gtk-entry
                                                :text "Sensitive Spinner"))
        (gtk-box-pack-start vbox hbox))
      (let ((hbox (make-instance 'gtk-box
                                 :orientation :horizontal
                                 :sensitive nil)))
        (gtk-box-pack-start hbox insensitive)
        (gtk-box-pack-start hbox (make-instance 'gtk-entry
                                                :text "Insensitive Spinner"))
        (gtk-box-pack-start vbox hbox))
      (let ((toggle (make-instance 'gtk-toggle-button
                                   :label "Stop animation"
                                   :margin-top 6)))
        (g-signal-connect toggle "toggled"
            (lambda (button)
              (if (gtk-toggle-button-active button)
                  (progn
                    (setf (gtk-button-label button) "Start animation")
                    (gtk-spinner-stop sensitive)
                    (gtk-spinner-stop insensitive))
                  (progn
                    (setf (gtk-button-label button) "Stop animation")
                    (gtk-spinner-start sensitive)
                    (gtk-spinner-start insensitive)))))
        (g-signal-emit toggle "toggled")
        (gtk-box-pack-start vbox toggle))
      (gtk-container-add window vbox)
      (gtk-widget-show-all window))))
