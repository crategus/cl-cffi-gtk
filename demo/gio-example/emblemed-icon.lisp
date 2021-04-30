;;;; Example Emblemed Icon (2021-4-27)

(in-package :gio-example)

(defun example-emblemed-icon ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :title "Example Emblemed Icon"
                                  :type :toplevel
                                  :border-width 12
                                  :default-width 300
                                  :default-height 200))
           (grid (make-instance 'gtk-grid
                                :row-homogeneous t
                                :column-spacing 24
                                :row-spacing 6))
           (icon (make-instance 'g-themed-icon
                                :name "desktop"))
           (emblemed-icon (make-instance 'g-emblemed-icon
                                         :gicon icon))
           (emblem (make-instance 'g-emblem
                                  :icon (make-instance 'g-themed-icon
                                                       :name
                                                       "emblem-ok-symbolic"))))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Add the emblem to the icon
      (g-emblemed-icon-add-emblem emblemed-icon emblem)
      ;; Pack and show the window
      (gtk-grid-attach grid (make-instance 'gtk-label
                                           :use-markup t
                                           :xalign 0.0
                                           :valign :center
                                           :label "<big><b>Icon</b></big>")
                            1 1 1 2)
      (gtk-grid-attach grid (make-instance 'gtk-image
                                           :icon-size 6
                                           :gicon icon)
                            3 2 1 1)
      (gtk-grid-attach grid (make-instance 'gtk-label
                                           :use-markup t
                                           :xalign 0.0
                                           :label "<big><b>Emblem</b></big>")
                            1 3 1 2)
      (gtk-grid-attach grid (make-instance 'gtk-image
                                           :icon-size 6
                                           :gicon (g-emblem-icon emblem))
                            3 4 1 1)
      (gtk-grid-attach grid (make-instance 'gtk-label
                                           :use-markup t
                                           :xalign 0.0
                                           :label "<big><b>Emblem</b></big>")
                            1 3 1 2)
      (gtk-grid-attach grid (make-instance 'gtk-label
                                           :use-markup t
                                           :xalign 0.0
                                           :label
                                           "<big><b>Emblemed Icon</b></big>")
                            1 5 1 2)
      (gtk-grid-attach grid (make-instance 'gtk-image
                                           :icon-size 6
                                           :gicon emblemed-icon)
                            3 6 1 1)
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
