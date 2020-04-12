(in-package :gtk-example)

(defun show-printer-info (printer)
  (format t "name           : ~A~%" (gtk-printer-name printer))
  (format t "description    : ~A~%" (gtk-printer-description printer))
  (format t "accepting-jobs : ~A~%" (gtk-printer-accepting-jobs printer))
  (format t "accepts-pdf    : ~A~%" (gtk-printer-accepts-pdf printer))
  (format t "accepts-ps     : ~A~%" (gtk-printer-accepts-ps printer))
  (format t "backend        : ~A~%" (gtk-printer-backend printer))
  (format t "icon-name      : ~A~%" (gtk-printer-icon-name printer))
  (format t "is-virtual     : ~A~%" (gtk-printer-is-virtual printer))
  (format t "job-count      : ~A~%" (gtk-printer-job-count printer))
  (format t "location       : ~A~%" (gtk-printer-location printer))
  (format t "paused         : ~A~%" (gtk-printer-paused printer))
  (format t "state-message  : ~A~%" (gtk-printer-state-message printer))
  (multiple-value-bind (top bottom left right)
      (gtk-printer-hard-margins printer)
    (format t "hard-margins   : ~A, ~A, ~A, ~A~%~%" top bottom left right)))

(defun example-print-dialog ()
  (let ((response nil)
        (settings (gtk-print-settings-new-from-file (rel-path "print-dialog.ini"))))
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
                            (setf settings
                                  (gtk-print-unix-dialog-print-settings dialog))
                            (gtk-widget-destroy dialog)))
        ;; Signal handler to print the selected printer
        (g-signal-connect dialog "notify::selected-printer"
            (lambda (widget param)
              (declare (ignore param))
              (let ((printer (gtk-print-unix-dialog-selected-printer widget)))
                (show-printer-info printer))))

        ;; Set the print settings and a size for the window.
        (setf (gtk-window-default-size dialog) '(350 200))
        (setf (gtk-print-unix-dialog-print-settings dialog) settings)
        ;; Show the dialog
        (gtk-widget-show-all dialog)))
    (join-gtk-main)
    (when (eq :ok (foreign-enum-keyword 'gtk-response-type response))
      (format t "~&Save print settings to ~A~%" (rel-path "print-dialog.ini"))
      (gtk-print-settings-to-file settings (rel-path "print-dialog.ini")))
    (format t "Back from message dialog with response-id : ~A~%" response)))

