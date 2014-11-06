(in-package :gtk-example)

(defun example-grab ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (make-instance 'gtk-window
                                 :title "Example Grab Widget"
                                 :type :toplevel
                                 :border-width 12))
          (hgrid (make-instance 'gtk-grid
                                :orientation :horizontal))
          (vgrid (make-instance 'gtk-grid
                                :orientation :vertical))
          (label (make-instance 'gtk-label
                                :label "Place for the info"))
          (button1 (make-instance 'gtk-toggle-button
                                  :label "I have the grab."))
          (button2 (make-instance 'gtk-button
                                  :label "You cannot press me."))
         )
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (g-signal-connect button1 "toggled"
          (lambda (widget)
            (if (gtk-toggle-button-active widget)
                (progn
                  (setf (gtk-button-label button1) "I have the grab")
                  (setf (gtk-button-label button2) "You cannot press me")
                  (gtk-grab-add widget))
                (progn
                  (setf (gtk-button-label button1) "Press me to grab me")
                  (setf (gtk-button-label button2) "You can press me.")
                  (gtk-grab-remove widget)))))
      (setf (gtk-toggle-button-active button1) t)

      (gtk-container-add vgrid button1)
      (gtk-container-add vgrid button2)
      (gtk-container-add hgrid label)
      (gtk-container-add hgrid vgrid)
      (gtk-container-add window hgrid)
      ;; Show the window.
      (gtk-widget-show-all window))))
