
(defun example-geometry-hints ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk-window-new :toplevel))
          (box (make-instance 'gtk-box :orientation :vertical))
          ;; Define a geometry for the size hints
          (size-hints (make-gdk-geometry :min-width        600
                                         :min-height       500
                                         :max-width          0
                                         :max-height         0
                                         :base-width       100
                                         :base-height       50
                                         :width-increment   50
                                         :height-increment  50
                                         :min-aspect         0.0d0
                                         :max-aspect         0.0d0
                                         :win-gravity :north-west)))

      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      ;; Add the box to the window
      (gtk-container-add window box)

      ;; Set the geometry hints
      (gtk-window-set-geometry-hints window
                                     window
                                     size-hints
                                     '(:min-size :base-size :resize-inc))
      ;; Show the window.
      (gtk-widget-show-all window))))
