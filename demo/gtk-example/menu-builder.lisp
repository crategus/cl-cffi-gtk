
(defun example-menu-builder ()
  (within-main-loop
      (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-get-default))
            nil)
      (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
            nil)
    (let ((builder (make-instance 'gtk-builder)))

      (gtk-builder-add-from-file builder "menu-builder.ui")
      (let ((window (gtk-builder-get-object builder "window")))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
      (gtk-widget-show-all window)))))

