;;;; Simple App Chooser Dialog

(in-package #:gtk-demo)

(defun demo-app-chooser-dialog ()
  (let ((file-name nil))
    (within-main-loop
      (let ((dialog (make-instance 'gtk-app-chooser-dialog
                                   :content-type "text/plain"
                                   :show-dialog-item t)))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            ;; Quit the main loop and destroy the thread
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
           (lambda (dialog response-id)
             (format t "in RESPONSE with ~A~%" response-id)
             ;; Check the response id from the file chooser dialog
             (when (eql response-id
                        ;; Convert the symbol :accept to the number value.
                        (foreign-enum-value 'gtk-response-type :accept))
               ;; Get the file name and store it.
               (setf file-name (gtk-file-chooser-filename dialog)))
             ;; Destroy the dialog.
             (gtk-widget-destroy dialog)))
        ;; Show the dialog.
        (gtk-widget-show dialog)))
    (when file-name
      (format t "~A~%" file-name))))
