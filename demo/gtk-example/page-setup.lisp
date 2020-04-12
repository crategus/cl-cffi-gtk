
(defun do-page-setup (window page-setup)

  (format t "in do-page-setup.~%")

  (let* ((settings (gtk-print-settings-new))

         (page-setup-new (gtk-print-run-page-setup-dialog window page-setup settings)))

         (format t "orientation ~A~%" (gtk-page-setup-get-orientation page-setup-new))
         (format t "paper size ~A~%" (gtk-page-setup-get-paper-size page-setup-new))
         (format t "top margin ~A~%" (gtk-page-setup-get-top-margin page-setup-new :mm))
         (format t "bottom margin ~A~%" (gtk-page-setup-get-bottom-margin page-setup-new :mm))
         (format t "left margin ~A~%" (gtk-page-setup-get-left-margin page-setup-new :mm))
         (format t "right margin ~A~%" (gtk-page-setup-get-right-margin page-setup-new :mm))
         (format t "paper width ~A~%" (gtk-page-setup-get-paper-width page-setup-new :mm))
         (format t "paper height ~A~%" (gtk-page-setup-get-paper-height page-setup-new :mm))
         (format t "page width ~A~%" (gtk-page-setup-get-page-width page-setup-new :mm))
         (format t "page height ~A~%" (gtk-page-setup-get-page-height page-setup-new :mm))

  ))


(defun example-page-setup ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
                                 :title "Example Page Setup"
                                 :type :toplevel
                                 :border-width 12))
          (button (make-instance 'gtk-button
                                 :label "Print"))
          (page-setup (gtk-page-setup-new)))

      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (g-signal-connect button "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Print button pressed.~%")
                          (do-page-setup window page-setup)

))

      (gtk-container-add window button)
      (gtk-widget-show-all window))))
