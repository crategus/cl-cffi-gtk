;;;; Simple Message Dialog - 2021-11-12

(in-package #:gtk-example)

(defun example-message-dialog-simple (&optional application)
  (let ((response))
    (within-main-loop
      (let ((dialog (make-instance 'gtk-message-dialog
                                   :application application
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
    (format t "Back from message dialog with response-id ~A~%" response)))
