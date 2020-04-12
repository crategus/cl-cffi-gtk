(in-package :gtk-example)

(defun example-page-setup-unix-dialog ()
  (let (response page-setup)
    (within-main-loop
      (let ((dialog (make-instance 'gtk-page-setup-unix-dialog
                                   :title "Example Page Setup Dialog"
                                   :default-height 250
                                   :default-width 400)))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
                          (lambda (dialog response-id)
                            (setf response response-id)
                            (setf page-setup
                                  (gtk-page-setup-unix-dialog-page-setup dialog))
                            (gtk-widget-destroy dialog)))
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (values response page-setup)))
