;;;; Examples for GtkMessageDialog

(defun example-message-dialog-new ()
  (let ((response))
    (within-main-loop
      (let ((dialog (gtk-message-dialog-new nil
                                            '(:modal)
                                            :info
                                            :ok
                                            "Info Message Dialog")))
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
        ;; Set secondary text
        (gtk-message-dialog-format-secondary-text dialog
                "This is a message dialog of type :info with a secondary text.")
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))

(defun example-message-dialog-new-with-markup ()
  (let ((response))
    (within-main-loop
      (let ((dialog (gtk-message-dialog-new-with-markup nil
                                            '(:modal)
                                            :info
                                            :ok
                                            "Info Message Dialog with <b>Markup</b>")))
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
        ;; Set secondary text
        (gtk-message-dialog-format-secondary-markup dialog
                "This is a <b>message dialog</b> of type :info with a secondary text.")
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))

(defun example-message-dialog-set-markup ()
  (let ((response))
    (within-main-loop
      (let ((dialog (gtk-message-dialog-new nil
                                            '(:modal)
                                            :info
                                            :ok
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
        ;; Set text with markup
        (gtk-message-dialog-set-markup dialog "Info Message Dialog with <b>Markup</b>")
        ;; Set secondary text
        (gtk-message-dialog-format-secondary-markup dialog
                "This is a <b>message dialog</b> of type :info with a secondary text.")
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))


(defun example-message-dialog-set-image ()
  (let ((response))
    (within-main-loop
      (let ((image (gtk-image-new-from-stock "gtk-ok" :dialog))
            (dialog (gtk-message-dialog-new nil
                                            '(:modal)
                                            :info
                                            :ok
                                            "Info Message Dialog")))
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
        ;; Set an image.
        (gtk-message-dialog-set-image dialog image)
        ;; Set secondary text.
        (gtk-message-dialog-format-secondary-text dialog
                "This is a message dialog of type :info with a different image.")
        ;; Show the dialog.
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))

(defun example-message-dialog-get-message-area ()
  (let ((response))
    (within-main-loop
      (let ((label (gtk-label-new "A label added to the message area."))
            (dialog (gtk-message-dialog-new nil
                                            '(:modal)
                                            :info
                                            :ok
                                            "Info Message Dialog")))
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
        ;; Set secondary text
        (gtk-message-dialog-format-secondary-text dialog
                "This is a message dialog of type :info with a secondary text.")
        ;; Add a label to the message area.
        (gtk-container-add (gtk-message-dialog-message-area dialog) label)
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id : ~A~%" response)))

