(in-package #:gtk-demo)

(defun demo-dialog-toplevel (message)
 (let ((response nil))
   (within-main-loop
    (let (;; Create the widgets
          (dialog (gtk-dialog-new-with-buttons "Demo Toplevel Dialog"
                                               nil ; No Parent window
                                               '(:modal)
                                               "gtk-ok"
                                               :none
                                               "gtk-cancel"
                                               :cancel))
          (label (gtk-label-new message)))
      ;; Signal handler for the dialog to handle the signal "destroy".
      (g-signal-connect dialog "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Quit the main loop and destroy the thread
                          (leave-gtk-main)))
      ;; Ensure that the dialog window is destroyed when the user responds.
      (g-signal-connect dialog "response"
                        (lambda (dialog response-id)
                          (setf response response-id)
                          (gtk-widget-destroy dialog)))
      ;; Add the label, and show everything we have added to the dialog.
      (gtk-container-add (gtk-dialog-get-content-area dialog) label)
      (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (when response
      (format t "The response ID is ~A" response))))


(defun demo-simple-file-chooser-dialog ()
  (let ((file-name nil))
    (within-main-loop
      (let ((dialog (gtk-file-chooser-dialog-new "Open File"
                                                 nil
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
             ;; Destroy the dialog.
             (gtk-widget-destroy dialog)))
        ;; Show the dialog.
        (gtk-widget-show dialog)))
    ;; Wait until the dialog is finished.
    (join-gtk-main)
    (when file-name
      (format t "~A~%" file-name))))

