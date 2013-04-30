;;;; Switch

(defun example-switch ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :type :toplevel
                                 :title "Example Switch"
                                 :default-width 230
                                 :border-width 12))
          (switch (make-instance 'gtk-switch
                                 :active t))
          (label (make-instance 'gtk-label
                                :label "The Switch is ON"))
          (grid (make-instance 'gtk-grid
                               :orientation :vertical
                               :row-spacing 6
                               :column-homogeneous t)))
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (g-signal-connect switch "notify::active"
         (lambda (widget param)
           (declare (ignore param))
           (if (gtk-switch-get-active widget)
               (gtk-label-set-label label "The Switch is ON")
               (gtk-label-set-label label "The Switch is OFF"))))
      (gtk-container-add grid switch)
      (gtk-container-add grid label)
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
