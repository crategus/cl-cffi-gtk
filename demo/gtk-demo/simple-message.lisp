;;;; Simple Message

(defun example-simple-message ()
  (let ((response))
    (within-main-loop
      (let ((dialog (make-instance 'gtk-message-dialog
                                   :message-type :info
                                   :buttons :ok
                                   :text "Info Message Dialog"
                                   :secondary-text
                                   (format nil
                                           "This is a message dialog of type ~
                                            :info with a secondary text."))))
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
        (gtk-widget-show dialog)))
;    (join-gtk-main)
    (format t "Back from message dialog with response-id ~A~%" response)))
