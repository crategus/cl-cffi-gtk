
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
            (if (gtk-toggle-button-get-active widget)
                (progn
                  (gtk-button-set-label button1 "I have the grab")
                  (gtk-button-set-label button2 "You cannot press me")
                  (gtk-grab-add widget))
                (progn
                  (gtk-button-set-label button1 "Press me to grab me")
                  (gtk-button-set-label button2 "You can press me.")
                  (gtk-grab-remove widget)))))
      (gtk-toggle-button-set-active button1 t)
  
      (gtk-container-add vgrid button1)
      (gtk-container-add vgrid button2)
      (gtk-container-add hgrid label)
      (gtk-container-add hgrid vgrid)
      (gtk-container-add window hgrid)
      ;; Show the window.
      (gtk-widget-show-all window))))
