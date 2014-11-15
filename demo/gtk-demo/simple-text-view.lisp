;;;; Simple Text View

(in-package #:gtk-demo)

(defun example-simple-text-view ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Simple Text View"
                                  :default-width 300))
           (view (make-instance 'gtk-text-view))
           (buffer (gtk-text-view-buffer view)))
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-text-buffer-set-text buffer "Hello, this is some text.")
      (gtk-container-add window view)
      (gtk-widget-show-all window))))

