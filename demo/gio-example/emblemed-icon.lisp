;;;; Example Emblemed Icon (2021-4-27)
;;;;
;;;; The g-emblemed-icon class is an implementation of the g-icon interface that
;;;; supports adding an emblem to an icon. Adding multiple emblems to an icon is
;;;; ensured via the function g-emblemed-icon-add-emblem.
;;;;
;;;; Note that the g-emblemed-icon class allows no control over the position of
;;;; the emblems. See also the g-emblem class for more information.

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
                                :column-spacing 24
                                :row-spacing 24))
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
                                           :label "<b>Icon</b>")
                            1 0 1 2)
      (gtk-grid-attach grid (make-instance 'gtk-image
                                           :icon-size 6
                                           :gicon icon)
                            3 1 1 1)
      (gtk-grid-attach grid (make-instance 'gtk-label
                                           :use-markup t
                                           :xalign 0.0
                                           :label "<b>Emblem</b>")
                            1 2 1 2)
      (gtk-grid-attach grid (make-instance 'gtk-image
                                           :icon-size 6
                                           :gicon (g-emblem-icon emblem))
                            3 3 1 1)
      (gtk-grid-attach grid (make-instance 'gtk-label
                                           :use-markup t
                                           :xalign 0.0
                                           :label
                                           "<b>Emblemed Icon</b>")
                            1 4 1 2)
      (gtk-grid-attach grid (make-instance 'gtk-image
                                           :icon-size 6
                                           :gicon emblemed-icon)
                            3 5 1 1)
      (gtk-container-add window grid)
      (gtk-widget-show-all window))))
