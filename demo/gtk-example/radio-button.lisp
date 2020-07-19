(in-package :gtk-example)

(defun example-radio-button ()
  (within-main-loop
    (let ((radio1 nil)
          (radio2 nil)
          (window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Radio Button"
                                 :border-width 12
                                 :default-width 300))
          (box (make-instance 'gtk-box
                              :orientation :vertical)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Create a radio button with a GtkEntry widget
      (setf radio1 (gtk-radio-button-new nil))
      (gtk-container-add radio1 (gtk-entry-new))
      ;; Create a radio button with a label
      (setf radio2
            (gtk-radio-button-new-with-label-from-widget radio1
                                                         "Second Radio Button"))
      ;; Pack them into a box, then show all the widgets
      (gtk-box-pack-start box radio1 :padding 6)
      (gtk-box-pack-start box radio2 :padding 6)
      (gtk-container-add window box)
      ;; Show the window.
      (gtk-widget-show-all window))))

