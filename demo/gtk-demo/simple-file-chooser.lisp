;;;; Simple File Chooser Dialog

(in-package #:gtk-demo)

(defun example-simple-file-chooser-dialog ()
  (let ((file-name nil))
    (within-main-loop
      (let ((dialog (gtk-file-chooser-dialog-new "Open File"
                                                 *demo-window*
                                                 :open
                                                 "gtk-cancel" :cancel
                                                 "gtk-open" :accept)))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            ;; Quit the main loop and destroy the thread
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
           (lambda (dialog response-id)
             ;; Check the response id from the file chooser dialog
             (when (eql response-id
                        ;; Convert the symbol :accept to the number value.
                        (foreign-enum-value 'gtk-response-type :accept))
               ;; Get the file name and store it.
               (setf file-name (gtk-file-chooser-get-filename dialog)))
               (if file-name
                   (format t "Get the file ~A~%" file-name)
                   (format t "The retrieval of a filename failed.~%"))
             ;; Destroy the dialog.
             (gtk-widget-destroy dialog)))
        ;; Show the dialog.
        (gtk-widget-show dialog)))))
