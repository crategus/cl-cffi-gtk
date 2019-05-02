
(defun example-print-dialog ()
  (let ((response))
    (within-main-loop
      (let ((dialog (gtk-print-unix-dialog-new "Example Print Dialog"
                                               nil)))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
                          (lambda (dialog response-id)
                            (setf response response-id)
                            (gtk-widget-destroy dialog)))
        ;; Set a size for the window.
        (setf (gtk-window-default-size dialog) '(350 200))
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))
