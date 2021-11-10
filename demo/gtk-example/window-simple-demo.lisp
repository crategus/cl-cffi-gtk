;;;; Example Simple Window - 2021-11-2
;;;;
;;;; This example shows a very simple window. The program creates a 200 x 200
;;;; pixel window. In this case the window has the default title "sbcl". The
;;;; window can be sized and moved.

(in-package :gtk-example)

(defun example-window-simple-demo (&optional application)
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk-window-new :toplevel)))
      ;; Add the window to the list of windows of the application
      (when application
        (setf (gtk-window-application window) application))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window))))
