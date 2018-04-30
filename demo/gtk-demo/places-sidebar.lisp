;;;; Places Sidebar

(in-package #:gtk-demo)

(defun example-places-sidebar ()
  (within-main-loop
    (let ((window (gtk-window-new :toplevel))
          (sidebar (make-instance 'gtk-places-sidebar)))
      (g-signal-connect sidebar "open-location"
                        (lambda (sidebar location open-flags)
                          (format T "want to open ~A with ~A flags~%"
                                  (g-file-get-parse-name location) open-flags)
                          (let ((shortcuts (gtk-places-sidebar-list-shortcuts sidebar)))
                            (format T "sidebar has ~D shortcuts~%" (length shortcuts)))))
      (gtk-container-add window sidebar)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window))))
