;;;; Example Stack - 2021-12-7
;;;;
;;;; GtkStack is a container that shows a single child at a time, with
;;;; nice transitions when the visible child changes.
;;;; GtkStackSwitcher adds buttons to control which child is visible.

(in-package :gtk-example)

(defun example-stack (&optional (application nil))
  (within-main-loop
    (let* ((builder (gtk-builder-new-from-file (sys-path "stack.ui")))
           (window (gtk-builder-object builder "window1")))
      (setf (gtk-window-application window) application)
      (g-signal-connect window "destroy"
                               (lambda (widget)
                                 (declare (ignore widget))
                                 (leave-gtk-main)))
      (gtk-widget-show-all window))))
